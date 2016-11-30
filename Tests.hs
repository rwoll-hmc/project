{-# LANGUAGE FlexibleContexts #-}

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Parsec as Parsec
import qualified AST
import qualified Parser
import Interp (fuzzyMatch)

main = defaultMain tests

pr p s = Parsec.parse p "Sample Input" s

tests :: TestTree
tests = testGroup "Unit Tests" [parserTests, interpTests]

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ characterTests,
    deptTests,
    cmdTests,
    actionTests,
    cueNumber
  ]

characterTests :: TestTree
characterTests = testGroup "Character(s)"
  [ testCase "Basic Character Name" $
      Parsec.parse Parser.character "" "@CECIL" @?= Right (AST.Character "CECIL"),
    testCase "Many Characters" $
      Parsec.parse Parser.characters "" (unlines ["Characters:", "  @RWOLL", "  @TEST"])
        @?= Right [AST.Character "RWOLL", AST.Character "TEST"]]

deptTests :: TestTree
deptTests = testGroup "Department(s)"
  [ testCase "Basic Department Name" $
      Parsec.parse Parser.department "" "#LX" @?= Right (AST.Department "LX"),
    testCase "Many Departments" $
      Parsec.parse Parser.departments "" (unlines ["Departments:", "  #LX", "  #SD"])
        @?= Right [AST.Department "LX", AST.Department "SD"]]

cmdTests :: TestTree
cmdTests = testGroup "Command Tests" $
  reflectTests (pr Parser.command) [
    ("GO", AST.Go, Nothing),
    ("STBY", AST.Stby, Nothing),
    ("WARN", AST.Warn, Nothing)
  ]

actionTests :: TestTree
actionTests = testGroup "Action Tests" $
  reflectTests (pr Parser.action) [
    ("ENTR", AST.Enter, Nothing),
    ("EXIT", AST.Exit, Nothing)
  ]

cueNumber :: TestTree
cueNumber = testGroup "Formatted Cue Number" $
  reflectTests (pr Parser.cueNumber) [
    ("{10}", 10, Nothing),
    ("{47}", 47, Nothing)
  ]

interpTests :: TestTree
interpTests = testGroup "Interpreter Tests" [fuzzyMatchTests]

fuzzyMatchTests :: TestTree
fuzzyMatchTests = testGroup "Fuzzy Match Tests"
  [ testGroup "Visual" $
      [
        testCase "Exactly Equal" $
          fuzzyMatch (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0)) (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0)) @?= True,
        testCase "Unspecified Index" $
          fuzzyMatch (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0)) (AST.Visual (AST.Character "CECIL") AST.Enter Nothing) @?= True,
        testCase "Different Characters (should fail)" $
          fuzzyMatch (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0)) (AST.Visual (AST.Character "ATHENA") AST.Enter (Just 0)) @?= False,
        testCase "Different Indexes (should fail)" $
          fuzzyMatch (AST.Visual (AST.Character "CECIL") AST.Enter (Just 1)) (AST.Visual (AST.Character "CECIL") AST.Enter (Just 0)) @?= False
      ],
    testGroup "Line" $
      [
        testCase "Exactly Equal" $
          fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0)) (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0)) @?= True,
        testCase "Substring Match Equal with Index" $
          fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0)) (AST.Line (AST.Character "CECIL") "Wo" (Just 0)) @?= True,
        testCase "Equal with Unspecified Index" $
          fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0)) (AST.Line (AST.Character "CECIL") "Hello, World!" Nothing) @?= True,
        testCase "Substring Match with Unspecified Index" $
          fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0)) (AST.Line (AST.Character "CECIL") "Wo" Nothing) @?= True,
        testCase "Equal Speech, Differing Indexes (should fail)" $
          fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0)) (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 1)) @?= False,
        testCase "Substring Match with Differing Indexes (should fail)" $
          fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0)) (AST.Line (AST.Character "CECIL") "Wo" (Just 1)) @?= False,
        testCase "Without Substring or Index (should fail)" $
          fuzzyMatch (AST.Line (AST.Character "CECIL") "Hello, World!" (Just 0)) (AST.Line (AST.Character "CECIL") "no match" Nothing) @?= False
      ]
  ]
reflectTests f = map (\(a, e, comment) -> testCase (genLabel comment a) $ f a @?= Right e) where
  genLabel (Just c) _ = c
  genLabel Nothing a = a
