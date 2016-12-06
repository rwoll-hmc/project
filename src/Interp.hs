-- | Interpreter for the DSL to generate `PromptScript`'s.
module Interp where

import qualified Text.Parsec         as Parsec
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
import Errors

-- | Compile a `CueSheet` into a `PromptScript` safely.
transpile :: CueSheet -> Either Error PromptScript
transpile cs = do
  declChars <- safeListToSet DuplicateCharacterDeclaration (characters cs)
  declDepts <- safeListToSet DuplicateDepartmentDeclaration (departments cs)
  foldlM insertAct (PromptScript "Untitled" declChars declDepts Map.empty) (acts cs)

-- | Inserts a `CueAct` into the appropriately place in a PromptScript.
insertAct :: PromptScript -> CueAct -> Either Error PromptScript
insertAct ps ca@(CueAct i scs) = do
  ensureStrictGtInsert (\_ -> OutOfOrderOrDuplicateActError ca) i (pActs ps)
  foldlM (insertScene i) ps scs

-- | Inserts a `CueScene` into a `PromptScript`.
insertScene :: Int                       -- ^ Act index of target.
            -> PromptScript              -- ^ Full `PromptScript` to be updated.
            -> CueScene                  -- ^ CueScene to be inserted.
            -> Either Error PromptScript -- ^ Updated `PromptScript` or `Error`.
insertScene aIdx ps sc@(CueScene i cgs) = do
  ensureStrictGtInsert (\_ -> OutOfOrderOrDuplicateSceneError sc) i
    (pActScenes $ findWithDefault (PromptAct Map.empty) aIdx (pActs ps))
  foldlM (insertCueGroups aIdx i) ps cgs

-- | Merge a prompt script with a cuesheet.
eval :: PromptScript                -- ^ The prompt script, possibly already marked up.
     -> PromptScript                -- ^ The compiled cuesheet.
     -> Either [Error] PromptScript -- ^ A list of `Error`s, or the updated prompt script.
eval prompt cuesheet =
  let (errs, prompt') = mapAccumWithKey (processAct cuesheet) [] (pActs prompt)
  in if null errs
       then Right $ prompt { pActs = prompt' }
       else Left errs

-- | Merge `PromptAct`s scenes into `PromptScript`.
processAct :: PromptScript         -- ^ The prompt script to be updated.
           -> [Error]              -- ^ Accumulating errors.
           -> Int                  -- ^ Act index.
           -> PromptAct            -- ^ Act from cue sheet to be merged in.
           -> ([Error], PromptAct) -- ^ Any errors and updated act.
processAct cuesheet errs0 actIdx (PromptAct scenes) =
  let (errs, scenes') = mapAccumWithKey (processScene cuesheet actIdx) [] scenes
  in if null errs
       then (errs0, PromptAct scenes')
       else (errs0 ++ errs, PromptAct scenes')

-- | Merge `PromptScenes`s cue groups into `PromptScript`.
processScene :: PromptScript           -- ^ The cuehseet
             -> Int                    -- ^ Act index
             -> [Error]                -- ^ Accumulating errors
             -> Int                    -- ^ Scene index
             -> PromptScene            -- ^ Original scene
             -> ([Error], PromptScene) -- ^ Results
processScene cuesheet actIdx errs0 scIdx (PromptScene markers) =
  -- `cms` are any relevant markers from cuesheet.
  let scenes = pActScenes $ findWithDefault (PromptAct Map.empty) actIdx (pActs cuesheet)
  in let cms = pMarkers $ findWithDefault (PromptScene Map.empty) scIdx scenes
     in let (errs, markers') = fst $ mapAccum resolveMarker ([], markers) cms
        in (errs0 ++ errs, PromptScene markers')

-- | Attempt to place a PromptMarker into a PromptScript.
resolveMarker :: ([Error], Map.Map Int PromptMarker)                             -- ^ Any errors and the "script".
              -> PromptMarker                                                    -- ^ PromptMarker to be placed.
              -> (([Error], Map.Map Int PromptMarker), Map.Map Int PromptMarker) -- ^ First is
                                                                                 -- updated, IGNORE
                                                                                 -- snd.
resolveMarker (errs0, m0) mFromCueSheet@(PromptMarker _ cues) =
  let res = filterWithKey (fuzzyMatch mFromCueSheet) m0
  in if null res
       then ((errs0 ++ [NoMatchError mFromCueSheet], m0), m0) -- TODO: check return: it is ignored
       else if Map.size res == 1
              then ((errs0, Map.adjust
                              (\p@(PromptMarker _ cs) -> p { pCues = cs ++ cues })
                              (head $ Map.keys res)
                              m0), m0)
              else ((errs0 ++ [AmbiguousError mFromCueSheet $ Map.elems res], m0), m0)

-- | Attempt to insert a CueGroup into a script.
insertCueGroups :: Int                       -- ^ Act index.
                -> Int                       -- ^ Scene index.
                -> PromptScript              -- ^ Prompt script to be updated.
                -> CueGroup                  -- ^ CueGroup to place
                -> Either Error PromptScript -- ^ Updated `PromptScript` or `Error`.
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

-- | Insert a PromptMarker into the script UNSAFELY.
insertPromptMarker :: Int                    -- ^ Act index.
                   -> Int                    -- ^ Scene index.
                   -> PromptMarker           -- ^ Marker to be inserted.
                   -> PromptScript           -- ^ Prompt script to be updated.
                   -> Either e PromptScript  -- ^ Updated prompt script.
insertPromptMarker aIdx scIdx m ps =
  -- TODO: self explanatory...
  let theScenes = (pActScenes $ findWithDefault (PromptAct Map.empty) aIdx (pActs ps))
  in let theScenes = (pActScenes $ findWithDefault (PromptAct Map.empty) aIdx (pActs ps))
     in let theMarkrs = (pMarkers $ findWithDefault (PromptScene Map.empty) scIdx theScenes)
        in let theMarkrs' = Map.insert (genKey theMarkrs) m theMarkrs
           in let theScene' = PromptScene theMarkrs'
              in let theAct' = PromptAct $ Map.insert scIdx theScene' theScenes
                 in let theActs' = Map.insert aIdx theAct' (pActs ps)
                    in Right $ ps { pActs = theActs' }

-- | Generator a fresh key to be used in a Map based on the highest index.
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

-- | Fuzzy check if two `PromptMarker`s match.
--
--
fuzzyMatch :: PromptMarker -- ^ Search Criteria
           -> Int          -- ^ Marker Index
           -> PromptMarker -- ^ Possible match
           -> Bool         -- ^ True if fuzzy match, False otherwise.
fuzzyMatch (PromptMarker (Visual _ _ _) _) _ (PromptMarker (Line _ _ _) _) = False
fuzzyMatch (PromptMarker (Line _ _ _) _) _ (PromptMarker (Visual _ _ _) _) = False
fuzzyMatch (PromptMarker (Line c1 l1 Nothing) _) _ (PromptMarker (Line c0 l0 _) _) =
  c0 == c1 && Utils.substring l1 l0
fuzzyMatch (PromptMarker (Line c1 l1 (Just i1)) _) i0 (PromptMarker (Line c0 l0 _) _) =
  c0 == c1 && i0 == i1 && Utils.substring l1 l0
fuzzyMatch (PromptMarker (Visual c1 a1 Nothing) _) _ (PromptMarker (Visual c0 a0 _) _) =
  c0 == c1 && a0 == a1
fuzzyMatch (PromptMarker v1@(Visual _ _ (Just _)) _) i0 (PromptMarker (Visual c0 a0 _) _) =
  (Visual c0 a0 $ Just i0) == v1
