module SampleScript where

import           AST
import qualified Data.Set as Set

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
              (replicate 5 a)

a = PromptAct 0 $ replicate 2 $
  PromptScene 47
    [ PromptMarker (Visual (Character "CECIL") Enter $ Just 0) []
    , PromptMarker (Line (Character "CECIL") "Hello, world!" $ Just 1) []
    ]
