module SampleScript where

import AST
import qualified Data.Set as Set

theScript :: PromptScript
theScript = PromptScript
  "DSLs: A Play"
  (Set.fromList [Character "ROSS", Character "PROF"])
  (Set.fromList [Department "LX", Department "SD"])
  (replicate 5 a)

a = PromptAct 0 $ replicate 2 $ PromptScene 47 [
    PromptMarker (Visual (Character "CECIL") Enter Nothing) $ [Cue (Department "LX") 10 Stby],
    PromptMarker (Line (Character "CECIL") "Hello, world! This is great and it is a really really long sentance that is so long it will probably have to wrap around the page in an infinite loop forever and ever and ever." Nothing) $ [Cue (Department "LX") 10 Stby, Cue (Department "SD") 20 Warn],
    PromptMarker (Line (Character "CECIL") "Hello, world! This is great and it is a really really long sentance that is so long it will probably have to wrap around the page in an infinite loop forever and ever and ever." Nothing) [],
    PromptMarker (Line (Character "CECIL") "Hello, world! This is great and it is a really really long sentance that is so long it will probably have to wrap around the page in an infinite loop forever and ever and ever." Nothing) $ [Cue (Department "LX") 10 Stby, Cue (Department "SD") 20 Warn],
    PromptMarker (Visual (Character "CECIL") Enter Nothing) []
  ]
