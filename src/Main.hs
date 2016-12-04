-- | Command line tool to process the DSL files.
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
import qualified Utils

-- | Level of verbosity of output.
verbose = True

-- | Process input file and out a LaTeX doc.
main :: IO ()
main = do
  c <- getContents
  header "Parsing Input..."
  parsed <- either ((>> exitFailure) . prntErrorC "Parsing Error") return
              (Parsec.parse Parser.cueSheet "stdin" c)

  header "Transpile..."
  parsed' <- either ((>> exitFailure) . prntErrorC "Transpiling Error") return (transpile parsed)
  evaled <- either ((>> exitFailure) . prntErrorC "Transpiling Error") return
              (eval theScript parsed')
  putStrLn $ show evaled
  header "Generating documents..."
  Renderer.main evaled
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
  putStrLn "ALL DONE! Ouput left in script.tex"
  setSGR [Reset]

-- | Prints a string to stdout when verbose mode on.
verbosePrnt :: String -> IO ()
verbosePrnt s = when verbose $ putStrLn s

-- | Generates a header line.
header :: String -> IO ()
header = verbosePrnt . Utils.fmtString

-- | Prints a string to sterr.
prntError :: String -> IO ()
prntError = hPutStrLn stderr

-- | Pretty print (with colors) an error message to stderr.
prntErrorC :: (Show a) => String -> a -> IO ()
prntErrorC hdr bdy = do
  hSetSGR stderr [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  prntError (hdr ++ ":")
  hSetSGR stderr [Reset]
  prntError $ unlines $ map ("  " ++) $ lines $ (show bdy)

-- | Print all errors and exit.
printAllErrorsAndExit :: (Traversable t, Show a) => (Either (t a) b) -> IO ()
printAllErrorsAndExit s =
  case s of
    (Left errs) -> mapM (prntError . show) errs >> do
                     setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
                     putStrLn "FAILURE"
                     setSGR [Reset] >> exitFailure
    _ -> return ()
