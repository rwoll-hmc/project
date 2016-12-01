module Interp where

import AST
import qualified Utils

fuzzyMatch :: Marker -- ^ Possible match
           -> Marker -- ^ Search Criteria
           -> Bool   -- ^ True if fuzzy match, False otherwise.
fuzzyMatch (Visual _ _ _) (Line _ _ _) = False
fuzzyMatch (Line _ _ _) (Visual _ _ _) = False
fuzzyMatch (Line c0 l0 (Just i0)) (Line c1 l1 Nothing) = c0 == c1 && Utils.substring l1 l0
fuzzyMatch (Line c0 l0 (Just i0)) (Line c1 l1 (Just i1)) = c0 == c1 && i0 == i1 && Utils.substring l1 l0
fuzzyMatch (Visual c0 a0 (Just i0)) (Visual c1 a1 Nothing) = c0 == c1 && a0 == a1
fuzzyMatch v0@(Visual _ _ (Just _)) v1@(Visual _ _ (Just _)) = v0 == v1
fuzzyMatch (Line _ _ Nothing) _ = error("TODO: Reimplement the data structure")
fuzzyMatch (Visual _ _ Nothing) _ = error("TODO: Reimplement the data structure")

isAmbiguous :: PromptScene -> CueGroup -> Bool
isAmbiguous p c = length (findOccurrences p c) > 1

findOccurrences :: PromptScene -> CueGroup -> [PromptMarker]
findOccurrences (PromptScene _ pms) (CueGroup m _) = filter (\pm -> fuzzyMatch (pMarker pm) m) pms

data Error
  = NoMatchError PromptScene CueGroup
  | AmbiguousError PromptScene CueGroup
  | DuplicateCharacterDeclaration Character
  | DuplicateDepartmentDeclaration Department deriving (Eq, Show)

placeCueInScene :: PromptScene -> CueGroup -> Either Error PromptScene
placeCueInScene ps cg = do
  res <- return $ findOccurrences ps cg
  case res of
    [] -> Left $ NoMatchError ps cg
    [c] -> return $ ps { pMarkers = foldr (\l acc -> if l `elem` res then l {pCues = pCues l ++ cgCues cg}:acc else l:acc) [] (pMarkers ps)}
    _ -> Left $ AmbiguousError ps cg

findDups :: Eq a => [a] -> [a]
findDups = snd . foldr (\l a@(seen, dups) -> if l `notElem` seen
                                             then (l:seen, dups)
                                             else if l `notElem` dups
                                               then (seen, l:dups)
                                               else (seen, dups)) ([],[])
