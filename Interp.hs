module Interp where

import           AST
import           Data.Foldable    (foldrM)
import           Data.Set         (Set, difference, elems, empty, insert,
                                   notMember)
import           Data.Traversable (mapM)
import qualified Utils

fuzzyMatch :: Marker -- ^ Possible match
           -> Marker -- ^ Search Criteria
           -> Bool   -- ^ True if fuzzy match, False otherwise.
fuzzyMatch (Visual _ _ _) (Line _ _ _) = False
fuzzyMatch (Line _ _ _) (Visual _ _ _) = False
fuzzyMatch (Line c0 l0 (Just i0)) (Line c1 l1 Nothing) = c0 == c1 && Utils.substring l1 l0
fuzzyMatch (Line c0 l0 (Just i0)) (Line c1 l1 (Just i1)) = c0 == c1 &&
                                                           i0 == i1 &&
                                                           Utils.substring l1 l0
fuzzyMatch (Visual c0 a0 (Just i0)) (Visual c1 a1 Nothing) = c0 == c1 && a0 == a1
fuzzyMatch v0@(Visual _ _ (Just _)) v1@(Visual _ _ (Just _)) = v0 == v1
fuzzyMatch (Line _ _ Nothing) _ = error ("TODO: Reimplement the data structure")
fuzzyMatch (Visual _ _ Nothing) _ = error ("TODO: Reimplement the data structure")

isAmbiguous :: PromptScene -> CueGroup -> Bool
isAmbiguous p c = length (findOccurrences p c) > 1

findOccurrences :: PromptScene -> CueGroup -> [PromptMarker]
findOccurrences (PromptScene _ pms) (CueGroup m _) = filter (\pm -> fuzzyMatch (pMarker pm) m) pms

data Error = NoMatchError PromptScene CueGroup
           | AmbiguousError PromptScene CueGroup
           | DuplicateCharacterDeclaration Character
           | DuplicateDepartmentDeclaration Department
           | UndeclaredCharacterError Character
           | UndeclaredDepartmentError Department
           | UnkownTargetCharacterError Character
  deriving (Eq, Show)

-- HACK: This is um......less than good. A refactoring of the data structures
--       is advised, but for now this (inefficient) tree-merging code will do.
mergePSCS :: PromptScript -> CueSheet -> Either [Error] PromptScript
mergePSCS ps cs = do
  psActs <- return $ pActs ps
  csActs <- return $ acts cs
  foldrM (\csAct ps -> mergeCSActPromptScript csAct ps) ps csActs

  where
    mergeCSActPromptScript :: CueAct -> PromptScript -> Either [Error] PromptScript
    mergeCSActPromptScript csAct ps = do
      updatedActs <- foldrM
                       (\pAct acc -> if pActId pAct == cueActIdx csAct
                                       then
                                       -- TODO: Factor out to helper.
                                       updateScenes (pActScenes pAct) (cueActScenes csAct) >>= (\updatedScenes -> return $ pAct { pActScenes = updatedScenes } : acc)
                                       else return $ pAct : acc)
                       []
                       (pActs ps) :: Either [Error] [PromptAct]
      return $ ps { pActs = updatedActs }

    updateScenes :: [PromptScene] -> [CueScene] -> Either [Error] [PromptScene]
    updateScenes pss css = mapM
                             (\pScene -> do
                                t <- return $ filter (\(CueScene i _) -> i == pSceneId pScene) css
                                if length t == 0
                                  then return pScene
                                  else placeCuesInScene pScene (head t))
                             pss

placeCuesInScene :: PromptScene -> CueScene -> Either [Error] PromptScene
placeCuesInScene sc cgs = (case foldr insertOne (sc, []) (csGroups cgs) of
                             (sc', []) -> Right sc'
                             (_, errs) -> Left errs)
  where
    insertOne :: CueGroup -> (PromptScene, [Error]) -> (PromptScene, [Error])
    insertOne cg (sc, errs) =
      case placeCueInScene sc cg of
        (Left e)    -> (sc, e : errs)
        (Right sc') -> (sc', errs)

placeCueInScene :: PromptScene -> CueGroup -> Either Error PromptScene
placeCueInScene ps cg = do
  res <- return $ findOccurrences ps cg
  case res of
    [] -> Left $ NoMatchError ps cg
    [c] -> return $ ps { pMarkers = foldr
                                      (\l acc -> if l `elem` res
                                                   then l { pCues = pCues l ++ cgCues cg } : acc
                                                   else l : acc)
                                      []
                                      (pMarkers ps) }
    _ -> Left $ AmbiguousError ps cg

findDups :: Eq a => [a] -> [a]
findDups = snd . foldr
                   (\l a@(seen, dups) -> if l `notElem` seen
                                           then (l : seen, dups)
                                           else if l `notElem` dups
                                                  then (seen, l : dups)
                                                  else (seen, dups))
                   ([], [])

checkDups :: (Eq a, Eq b) => (a -> b) -> [a] -> Either [b] ()
checkDups fErr ls = zeroOrErr $ map fErr $ findDups ls

checkDupChars :: [Character] -> Either [Error] ()
checkDupChars = checkDups DuplicateCharacterDeclaration

checkDupDepts :: [Department] -> Either [Error] ()
checkDupDepts = checkDups DuplicateDepartmentDeclaration

checkUndecl :: (Eq a, Ord a, Eq b) => (a -> b) -> Set a -> [a] -> Either [b] ()
checkUndecl fErr decl ls =
  let errs = foldr
               (\l acc -> if l `notMember` decl
                            then fErr l : acc
                            else acc)
               []
               ls
  in zeroOrErr errs

checkUndeclChars :: Set Character -> [Character] -> Either [Error] ()
checkUndeclChars decls ls = checkUndecl UndeclaredCharacterError decls ls

checkUndeclDept :: Set Department -> [Department] -> Either [Error] ()
checkUndeclDept decls ls = checkUndecl UndeclaredDepartmentError decls ls

zeroOrErr :: Eq a => [a] -> Either [a] ()
zeroOrErr ls = if length ls == 0
                 then Right ()
                 else Left ls

collectDepts :: CueScene -> [Department]
collectDepts (CueScene _ gs) = map department $ concatMap cgCues gs

collectChars :: CueScene -> [Character]
collectChars (CueScene _ gs) = map collectChar gs

collectCharsAndDepts :: CueSheet -> ([Character], [Department])
collectCharsAndDepts s = foldr
                           (\l (cs, ds) -> (collectChars l ++ cs, collectDepts l ++ ds))
                           ([], [])
                           (concatMap cueActScenes $ acts s)

checkForUnknownChars :: Set Character -> Set Character -> Either [Error] ()
checkForUnknownChars act exp = zeroOrErr $ map UnkownTargetCharacterError $ elems $ exp `difference` act

collectChar :: CueGroup -> Character
collectChar (CueGroup (Line c _ _) _) = c
collectChar (CueGroup (Visual c _ _) _) = c
