module Interp where

import AST (Marker(..), PromptScene(..), CueGroup(..), pMarkers, pMarker)
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
isAmbiguous _ (CueGroup (Visual _ _ (Just _)) _) = False
isAmbiguous _ (CueGroup (Line _ _ (Just _)) _) = False
isAmbiguous (PromptScene _ pms) (CueGroup m _) = length (filter (\pm -> fuzzyMatch (pMarker pm) m) pms) > 1
