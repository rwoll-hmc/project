import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Parsec as Parsec
import qualified AST
import qualified Parser

main = defaultMain tests

tests :: TestTree
tests = testGroup "Character Parser"
  [ testCase "Basic Character Name" $
      Parsec.parse Parser.character "" "@CECIL" @?= Right (AST.Character "CECIL"),
    testCase "Many Characters" $
      Parsec.parse Parser.characters "" (unlines ["Characters:", "  @RWOLL", "  @TEST"]) @?= Right [AST.Character "RWOLL", AST.Character "TEST"]]
