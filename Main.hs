module Main where

import           AST
import           Control.Monad
import qualified Data.Set            as Set
import           Interp
import qualified Parser
import qualified Renderer
import           SampleScript
import           System.Console.ANSI
import           System.Exit
import           System.IO
import qualified Text.Parsec         as Parsec

fmtString s = unlines [divider, padText s, divider]
  where
    divider = replicate 80 '='
    padText t = "==== " ++ t ++ " " ++ replicate (76 - length s - 2) '='

verbose = True

main :: IO ()
main = do
  -- TODO: Add CLI switches and custom i/o files
  c <- getContents

  header "Parsing Input..."
  parsed <- (case Parsec.parse Parser.cueSheet "stdin" c of
               (Left e)  -> hPutStrLn stderr ("Error while parsing:\n\n" ++ show e) >> exitFailure
               (Right r) -> putStrLn (show r ++ "--- END ---") >> return r)

  header "Checking for duplicate Character declarations..."
  printAllErrorsAndExit $ checkDupChars $ characters parsed

  header "Checking for duplicate Department declarations..."
  printAllErrorsAndExit $ checkDupDepts $ departments parsed

  (chars, depts) <- return $ collectCharsAndDepts parsed

  header "Checking for undeclared Characters..."
  printAllErrorsAndExit $ checkUndeclChars (Set.fromList $ characters parsed) chars

  header "Checking for undeclared Departments..."
  printAllErrorsAndExit $ checkUndeclDept (Set.fromList $ departments parsed) depts

  header "Checking for unkown Character References..."
  printAllErrorsAndExit $ checkForUnknownChars (pCharacters theScript) $ Set.fromList $ characters
                                                                                          parsed

  {- TODO: Check for sequential ordering of cues and logical STBY|WARN -> GO.
     TODO: Check for undeclared scenes and acts.
     TODO: Check for duplicated scenes and acts. -}
  -- FIXME: remove this line after hfmt
  header "Placing cues..."
  printAllErrorsAndExit $ mergePSCS theScript parsed

  (case mergePSCS theScript parsed of
     (Right s) -> Renderer.main s
     _         -> return ())

  header "Generating documents..."
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
  -- TODO: Add variable name for output.
  putStrLn "ALL DONE! Ouput left in simple.tex"
  -- TODO: Call process for latexmk
  setSGR [Reset]

verbosePrnt :: String -> IO ()
verbosePrnt s = when verbose $ putStrLn s

header :: String -> IO ()
header = verbosePrnt . fmtString

prntError :: String -> IO ()
prntError = hPutStrLn stderr

printAllErrorsAndExit :: (Traversable t, Show a) => (Either (t a) b) -> IO ()
printAllErrorsAndExit s =
  case s of
    (Left errs) -> mapM (prntError . show) errs >> do
                     setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
                     putStrLn "FAILURE"
                     setSGR [Reset] >> exitFailure
    _ -> return ()
