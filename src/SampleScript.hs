-- | A sample script to user for presentations/examples.
module SampleScript where

import           AST
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

-- | A sample script.
theScript :: PromptScript
theScript = PromptScript
              "DSLs: A Play"
              (Set.fromList
                 [ Character "ROSS"
                 , Character "PROF"
                 , Character "SARAH"
                 , Character "CECIL"
                 , Character "ATHEN"
                 ])
              (Set.fromList [Department "LX", Department "SD"])
              (Map.fromList [(0, a)])
  where
    a = PromptAct $ Map.fromList [(0, PromptScene $ Map.fromList lines')]
    lines' = [ (0, PromptMarker (Visual (Character "CECIL") Enter Nothing) [])
             , (1, PromptMarker (Line (Character "CECIL") "Hello, world!" Nothing) [])
             , (2, PromptMarker (Line (Character "CECIL") "Hello, world!" Nothing) [])
             ]