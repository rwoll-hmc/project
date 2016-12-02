{-# LANGUAGE FlexibleContexts #-}

import           Test.Tasty
import           Test.Tasty.HUnit

import           AST
import           Data.Set         as Set
import           Interp
import qualified Parser
import qualified Text.Parsec      as Parsec

main = defaultMain tests

pr p s = Parsec.parse p "Sample Input" s

tests :: TestTree
tests = testGroup "Unit Tests" [parserTests] -- , interpTests]

parserTests :: TestTree
parserTests = testGroup "Parser Tests" [characterTests, deptTests, cmdTests, actionTests, cueNumber]

characterTests :: TestTree
characterTests = testGroup "Character(s)"
                   [ testCase "Basic Character Name" $
                     Parsec.parse Parser.character "" "@CECIL" @?= Right (AST.Character "CECIL")
                   , testCase "Many Characters" $
                     Parsec.parse Parser.characters ""
                       (unlines ["Characters:", "  @RWOLL", "  @TEST"])
                     @?= Right [AST.Character "RWOLL", AST.Character "TEST"]
                   ]

deptTests :: TestTree
deptTests = testGroup "Department(s)"
              [ testCase "Basic Department Name" $
                Parsec.parse Parser.department "" "#LX" @?= Right (AST.Department "LX")
              , testCase "Many Departments" $
                Parsec.parse Parser.departments "" (unlines ["Departments:", "  #LX", "  #SD"])
                @?= Right [AST.Department "LX", AST.Department "SD"]
              ]

cmdTests :: TestTree
cmdTests = testGroup "Command Tests" $
  reflectTests (pr Parser.command)
    [("GO", AST.Go, Nothing), ("STBY", AST.Stby, Nothing), ("WARN", AST.Warn, Nothing)]

actionTests :: TestTree
actionTests = testGroup "Action Tests" $
  reflectTests (pr Parser.action) [("ENTR", AST.Enter, Nothing), ("EXIT", AST.Exit, Nothing)]

cueNumber :: TestTree
cueNumber = testGroup "Formatted Cue Number" $
  reflectTests (pr Parser.cueNumber) [("{10}", 10, Nothing), ("{47}", 47, Nothing)]

-- interpTests :: TestTree interpTests = testGroup "Interpreter Tests"
--                 [ fuzzyMatchTests
--                 , isAmbiguousTests
--                 , placeCueInSceneTests
--                 , findDupTests
--                 , zeroOrErrTests
--                 , checkDupTests
--                 , checkUndeclTests
--                 ]
--
-- fuzzyMatchTests :: TestTree fuzzyMatchTests = testGroup "Fuzzy Match Tests"
--                     [ testGroup "Visual" $
--                       [ testCase "Exactly Equal" $
--                         fuzzyMatch (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0))
--                           (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0)) @?= True
--                       , testCase "Unspecified Index" $
--                         fuzzyMatch (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0))
--                           (AST.Visual (AST.Character "CECIL") AST.Enter Nothing) @?= True
--                       , testCase "Different Characters (should fail)" $
--                         fuzzyMatch (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0))
--                           (AST.Visual (AST.Character "ATHENA") AST.Enter (Just 0)) @?= False
--                       , testCase "Different Indexes (should fail)" $
--                         fuzzyMatch (AST.Visual (AST.Character "CECIL") AST.Enter (Just 1))
--                           (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0)) @?= False
--                       ]
--                     , testGroup "Line" $
--                       [ testCase "Exactly Equal" $
--                         fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0))
--                           (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0)) @?= True
--                       , testCase "Substring Match Equal with Index" $
--                         fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0))
--                           (AST.Line (AST.Character "CECIL") "Wo" (Just 0)) @?= True
--                       , testCase "Equal with Unspecified Index" $
--                         fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0))
--                           (AST.Line (AST.Character "CECIL") "Hello, World!" Nothing) @?= True
--                       , testCase "Substring Match with Unspecified Index" $
--                         fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0))
--                           (AST.Line (AST.Character "CECIL") "Wo" Nothing) @?= True
--                       , testCase "Equal Speech, Differing Indexes (should fail)" $
--                         fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0))
--                           (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 1)) @?= False
--                       , testCase "Substring Match with Differing Indexes (should fail)" $
--                         fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0))
--                           (AST.Line (AST.Character "CECIL") "Wo" (Just 1)) @?= False
--                       , testCase "Without Substring or Index (should fail)" $
--                         fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0))
--                           (AST.Line (AST.Character "CECIL") "no match" Nothing) @?= False
--                       ]
--                     ]
--
-- mockScene = PromptScene 47
--               [ PromptMarker (Visual (Character "CECIL") Enter (Just 0)) []
--               , PromptMarker (Line (Character "CECIL") "Hello, world!" (Just 1)) []
--               , PromptMarker (Line (Character "CECIL") "Hello, dsls!" (Just 2)) []
--               , PromptMarker (Line (Character "CECIL") "unique" (Just 3)) []
--               , PromptMarker (Visual (Character "CECIL") Exit (Just 4)) []
--               , PromptMarker (Visual (Character "CECIL") Enter (Just 5)) []
--               ]
--
-- mockScene0 = PromptScene 47 [PromptMarker (Visual (Character "CECIL") Enter (Just 0)) []]
--
-- cueGroup0 = CueGroup (Visual (Character "CECIL") Enter (Just 0)) [Cue (Department "LX") 99 Go]
--
-- eMockScene0 = PromptScene 47
--                 [ PromptMarker (Visual (Character "CECIL") Enter (Just 0))
--                     [Cue (Department "LX") 99 Go]
--                 ]
--
-- mockScene1 = PromptScene 47 [PromptMarker (Line (Character "CECIL") "Hello, world!" (Just 0)) []]
--
-- cueGroup1 = CueGroup (Line (Character "CECIL") "ello" Nothing) [Cue (Department "LX") 99 Go]
--
-- eMockScene1 = PromptScene 47
--                 [ PromptMarker (Line (Character "CECIL") "Hello, world!" (Just 0))
--                     [Cue (Department "LX") 99 Go]
--                 ]
--
-- mockScene2 = PromptScene 47
--                [ PromptMarker (Line (Character "CECIL") "Hello, world!" (Just 0))
--                    [Cue (Department "SD") 99 Go]
--                ]
--
-- cueGroup2 = CueGroup (Line (Character "CECIL") "ello" Nothing) [Cue (Department "LX") 99 Go]
--
-- eMockScene2 = PromptScene 47
--                 [ PromptMarker (Line (Character "CECIL") "Hello, world!" (Just 0))
--                     [Cue (Department "SD") 99 Go, Cue (Department "LX") 99 Go]
--                 ]
--
-- isAmbiguousTests :: TestTree isAmbiguousTests = testGroup "Ambiguous Checker" $
--   [ testGroup "Visual" $
--     [ testCase "Ambiguous with No Occurrence Specified" $
--       isAmbiguous mockScene (CueGroup (Visual (Character "CECIL") Enter Nothing) []) @?= True
--     , testCase "Ambiguity Fixed with Index" $
--       isAmbiguous mockScene (CueGroup (Visual (Character "CECIL") Enter (Just 0)) []) @?= False
--     , testCase "Unambiguous" $
--       isAmbiguous mockScene (CueGroup (Visual (Character "CECIL") Exit Nothing) []) @?= False
--     ]
--   , testGroup "Line" $
--     [ testCase "Ambiguous with No Occurrence Specified" $
--       isAmbiguous mockScene (CueGroup (Line (Character "CECIL") "Hello" Nothing) []) @?= True
--     , testCase "Ambiguity Fixed with Index" $
--       isAmbiguous mockScene (CueGroup (Line (Character "CECIL") "Hello" (Just 2)) []) @?= False
--     , testCase "Unambiguous" $
--       isAmbiguous mockScene (CueGroup (Line (Character "CECIL") "unique" Nothing) []) @?= False
--     ]
--   ]
--
-- placeCueInSceneTests :: TestTree placeCueInSceneTests = testGroup "Place Cue in Scene Tests" $
--   [ testCase "Place CueGroup with No Cues" $
--     placeCueInScene mockScene (CueGroup (Line (Character "CECIL") "Hello" (Just 2)) [])
--     @?= Right mockScene
--   , testCase "Expect Error for Cue without an Existent Target" $
--     placeCueInScene mockScene
--       (CueGroup (Line (Character "CECIL") "this isn't in the scrip" (Just 2)) [])
--     @?= Left
--           (NoMatchError mockScene
--              (CueGroup (Line (Character "CECIL") "this isn't in the scrip" (Just 2)) []))
--   , testCase "Expect Error for Ambiguous Placements" $
--     placeCueInScene mockScene (CueGroup (Line (Character "CECIL") "ello" Nothing) [])
--     @?= Left (AmbiguousError mockScene (CueGroup (Line (Character "CECIL") "ello" Nothing) []))
--   , testCase "Successfully Place Unambiguos Cue" $
--     placeCueInScene mockScene0 cueGroup0 @?= Right eMockScene0
--   , testCase "Place Ambiguous Cue" $
--     placeCueInScene mockScene1 cueGroup1 @?= Right eMockScene1
--   , testCase "Ensure Successfully Places Cue Appends" $
--     placeCueInScene mockScene2 cueGroup2 @?= Right eMockScene2
--   ]
--
-- findDupTests :: TestTree findDupTests = testGroup "Find Duplicates" $
--   [ testCase "Empty List" $ findDups "" @?= ""
--   , testCase "One Duplicate" $ findDups "abca" @?= "a"
--   , testCase "Many Duplicates" $ findDups "ababc" @?= "ab"
--   , testCase "Many Many Duplicates" $ findDups "abc6788671003" @?= "6780"
--   ]
--
-- zeroOrErrTests :: TestTree zeroOrErrTests = testGroup "Zero or Error"
--                    [ testCase "No Errors" (zeroOrErr ([] :: [Int])
--                                            @?= Right ())
--                    , testCase "Errors" $ zeroOrErr ["a", "b", "c"]
--                                          @?= Left ["a", "b", "c"]
--                    ]
--
-- checkDupTests :: TestTree checkDupTests = testGroup "Duplicate Check Tests" $
--   [ testCase "No Errors" $ (checkDups id ([1, 2, 3] :: [Int])) @?= Right ()
--   , testCase "Errors" $ checkDups (\x -> Just x) ["a", "b", "c", "a", "b"]
--                         @?= Left [Just "a", Just "b"]
--   ]
--
-- checkUndeclTests :: TestTree checkUndeclTests = testGroup "Undeclared Usage" $
--   [ testCase "No Errors" $ checkUndecl id (Set.fromList ["a"]) ["a"] @?= Right ()
--   , testCase "Errors" $ checkUndecl id (Set.fromList ["c"]) ["a", "a", "b", "c"]
--                         @?= Left ["a", "a", "b"]
--   ]
--
reflectTests f = Prelude.map (\(a, e, comment) -> testCase (genLabel comment a) $ f a @?= Right e)
  where
    genLabel (Just c) _ = c
    genLabel Nothing a = a
