module Interp where

import           AST
import           Control.Monad
import           Data.Foldable    (foldlM)
import           Data.Map.Strict  (Map, filterWithKey, findWithDefault,
                                   mapAccum, mapAccumWithKey)
import qualified Data.Map.Strict  as Map
import           Data.Set         (Set, difference, elems, empty, insert,
                                   notMember)
import           Data.Traversable (mapM)
import qualified Utils

transpile :: CueSheet -> Either Error PromptScript
transpile cs = do
  declChars <- safeListToSet DuplicateCharacterDeclaration (characters cs)
  declDepts <- safeListToSet DuplicateDepartmentDeclaration (departments cs)
  foldlM insertAct (PromptScript "Untitled" declChars declDepts Map.empty) (acts cs)

insertAct :: PromptScript -> CueAct -> Either Error PromptScript
insertAct ps ca@(CueAct i scs) = do
  ensureStrictGtInsert (\_ -> OutOfOrderOrDuplicateActError ca) i (pActs ps)
  foldlM (insertScene i) ps scs

insertScene :: Int -> PromptScript -> CueScene -> Either Error PromptScript
insertScene aIdx ps sc@(CueScene i cgs) = do
  ensureStrictGtInsert (\_ -> OutOfOrderOrDuplicateSceneError sc) i
    (pActScenes $ findWithDefault (PromptAct Map.empty) aIdx (pActs ps))
  foldlM (insertCueGroups aIdx i) ps cgs

eval :: PromptScript -> PromptScript -> Either [Error] PromptScript
eval prompt cuesheet =
  let (errs, prompt') = mapAccumWithKey (processAct cuesheet) [] (pActs prompt)
  in if null errs
       then Right $ prompt { pActs = prompt' }
       else Left errs

processAct :: PromptScript -> [Error] -> Int -> PromptAct -> ([Error], PromptAct)
processAct cuesheet errs0 actIdx (PromptAct scenes) =
  let (errs, scenes') = mapAccumWithKey (processScene cuesheet actIdx) [] scenes
  in if null errs
       then (errs0, PromptAct scenes')
       else (errs0 ++ errs, PromptAct scenes')

processScene :: PromptScript            -- ^ cuehseet
             -> Int                     -- ^ act index
             -> [Error]                 -- ^ accumulating errors
             -> Int                     -- ^ scene index
             -> PromptScene             -- ^ original scene
             -> ([Error], PromptScene)  -- ^ results
processScene cuesheet actIdx errs0 scIdx (PromptScene markers) =
  -- cms ::= any relavent markers from cue sheet
  let cms = pMarkers $ findWithDefault (PromptScene Map.empty) scIdx $ pActScenes $ findWithDefault
                                                                                      (PromptAct
                                                                                         Map.empty)
                                                                                      actIdx
                                                                                      (pActs
                                                                                         cuesheet)
  in let (errs, markers') = fst $ mapAccum resolveMarker ([], markers) cms
     in (errs0 ++ errs, PromptScene markers')

resolveMarker :: ([Error], Map.Map Int PromptMarker) -> PromptMarker -> (([Error], Map.Map Int PromptMarker), Map.Map Int PromptMarker)
resolveMarker (errs0, m0) mFromCueSheet@(PromptMarker _ cues) =
  let res = filterWithKey (fuzzyMatch mFromCueSheet) m0
  in if null res
       then ((errs0 ++ [NoMatchError mFromCueSheet], m0), m0) -- TODO: check return???
       else if Map.size res == 1
              then ((errs0, Map.adjust
                              (\p@(PromptMarker _ cs) -> p { pCues = cs ++ cues })
                              (head $ Map.keys res)
                              m0), m0)
              else ((errs0 ++ [AmbiguousError mFromCueSheet $ Map.elems res], m0), m0)

insertCueGroups :: Int -> Int -> PromptScript -> CueGroup -> Either Error PromptScript
insertCueGroups aIdx scIdx ps (CueGroup m cues) = do
  -- check character well-scoped
  (let char = mChar m
   in if char `notMember` pCharacters ps
        then Left (UndeclaredCharacterError char)
        else Right ())
  -- check department all well scoped
  foldM_
    (\() (Cue d _ _ _) -> if d `notMember` pDepartments ps
                            then Left (UndeclaredDepartmentError d)
                            else Right ())
    ()
    cues
  -- all checks pass, let's add it!
  insertPromptMarker aIdx scIdx (PromptMarker m cues) ps

insertPromptMarker :: Int -> Int -> PromptMarker -> PromptScript -> Either e PromptScript
insertPromptMarker aIdx scIdx m ps      -- TODO: Clean up this function; no need for the verbosity or the
                                        -- do
 = do
  theScenes <- return (pActScenes $ findWithDefault (PromptAct Map.empty) aIdx (pActs ps))
  theMarkrs <- return (pMarkers $ findWithDefault (PromptScene Map.empty) scIdx theScenes)
  theMarkrs' <- return $ Map.insert (genKey theMarkrs) m theMarkrs
  theScene' <- return $ PromptScene theMarkrs'
  theAct' <- return $ PromptAct $ Map.insert scIdx theScene' theScenes
  theActs' <- return $ Map.insert aIdx theAct' (pActs ps)
  return $ ps { pActs = theActs' }

genKey :: (Enum k, Bounded k) => Map.Map k v -> k
genKey m
  | null m = minBound
  | otherwise = succ $ fst (Map.findMax m)

-- | Safely convert a list of elements to a set, complaining if there are any
--   duplicates.
safeListToSet :: (Ord a, Traversable t) => (a -> e) -> t a -> Either e (Set a)
safeListToSet eF = foldlM (flip handlePossibleDup) empty
  where
    handlePossibleDup e es = if e `elem` es
                               then Left (eF e)
                               else Right (e `insert` es)

-- | Check to see if the item attempting to be inserted, is strictly greater
--   than the highest element in the map.
ensureStrictGtInsert :: Ord k => (k -> e) -> k -> Map k v -> Either e (Map k v)
ensureStrictGtInsert eF i m
  | null m = Right m
  | otherwise = if i > fst (Map.findMax m)
                  then Right m
                  else Left (eF i)

fuzzyMatch :: PromptMarker -- ^ Search Criteria
           -> Int          -- ^ Marker Index
           -> PromptMarker -- ^ Possible match
           -> Bool         -- ^ True if fuzzy match, False otherwise.
fuzzyMatch (PromptMarker (Visual _ _ _) _) _ (PromptMarker (Line _ _ _) _) = False
fuzzyMatch (PromptMarker (Line _ _ _) _) _ (PromptMarker (Visual _ _ _) _) = False
fuzzyMatch (PromptMarker (Line c1 l1 Nothing) _) _ (PromptMarker (Line c0 l0 _) _) = c0 == c1 && Utils.substring
                                                                                                   l1
                                                                                                   l0

fuzzyMatch (PromptMarker (Line c1 l1 (Just i1)) _) i0 (PromptMarker (Line c0 l0 _) _) = c0 == c1 &&
                                                                                        i0 == i1 &&
                                                                                        Utils.substring
                                                                                          l1
                                                                                          l0
fuzzyMatch (PromptMarker (Visual c1 a1 Nothing) _) _ (PromptMarker (Visual c0 a0 _) _) = c0 == c1 && a0 == a1
fuzzyMatch (PromptMarker v1@(Visual _ _ (Just _)) _) i0 (PromptMarker (Visual c0 a0 _) _) = (Visual
                                                                                               c0
                                                                                               a0 $ Just
                                                                                                      i0) == v1

data Error = NoMatchError PromptMarker
           | AmbiguousError PromptMarker [PromptMarker]
           | DuplicateCharacterDeclaration Character
           | DuplicateDepartmentDeclaration Department
           | UndeclaredCharacterError Character
           | UndeclaredDepartmentError Department
           | UnkownTargetCharacterError Character
           | OutOfOrderOrDuplicateActError CueAct
           | OutOfOrderOrDuplicateSceneError CueScene
  deriving (Eq, Show)
