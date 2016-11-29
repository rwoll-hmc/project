{-# LANGUAGE FlexibleContexts #-}

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Parsec as Parsec
import qualified AST
import qualified Parser

main = defaultMain tests

pr p s = Parsec.parse p "Sample Input" s

tests :: TestTree
tests = testGroup "Parser Tests"
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

reflectTests f = map (\(a, e, comment) -> testCase (genLabel comment a) $ f a @?= Right e) where
  genLabel (Just c) _ = c
  genLabel Nothing a = a
