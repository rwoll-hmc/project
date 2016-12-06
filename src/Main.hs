-- | Command line tool to process the DSL files.
module Main where

import           AST
import           Control.Applicative
import           Control.Monad
import qualified Data.Set                 as Set
import           Interp
import qualified Parser
import qualified Renderer
import           SampleScript
import           System.Console.ANSI
import           System.Console.ArgParser
import           System.Exit
import           System.IO
import qualified Text.Parsec              as Parsec
import qualified Utils
import Errors

-- | Encapsulation of all command line options and switches.
data CLIArgs = CLIArgs { infile :: String, outfile :: String, silent :: Bool, latex :: Bool }
  deriving Show

-- | `cliParser` parses command-line options for the program.
cliParser :: ParserSpec CLIArgs
cliParser = CLIArgs
  `parsedBy` optPos   "-"           "infile"       `Descr` "Source to input file. By default this is stdin."
  `andBy`    optPos   "script.tex"  "outfile"      `Descr` "Destination to output file. By default stdout."
  `andBy`    boolFlag               "silent"       `Descr`  "Turn off the verbosity"
  `andBy`    boolFlag               "latex-off"    `Descr` "Turns off automatic call to latexmk"

-- | Parse the command line input and run the logic of the program.
main :: IO ()
main = withParseResult cliParser runner

-- | Run the logic of the program.
runner :: CLIArgs -> IO ()
runner s = do

  c <- if infile s == "-" then getContents else readFile $ infile s
  parsed <- either ((>> exitFailure) . prntErrorC "Parsing Error") return
              (Parsec.parse Parser.cueSheet (infile s) c)
  parsed' <- either ((>> exitFailure) . prntErrorC "Compiling Error") return (transpile parsed)
  evaled <- either ((>> exitFailure) . prntErrorC "Processing Error") return
              (eval theScript parsed')

  Renderer.main (outfile s) evaled

  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
  putStrLn $ "ALL DONE! Ouput left in " ++ outfile s
  setSGR [Reset]

prntError :: String -> IO ()
prntError = hPutStrLn stderr

-- | Pretty print (with colors) an error message to stderr.
prntErrorC :: (PrettyError a) => String -> a -> IO ()
prntErrorC hdr bdy = do
  hSetSGR stderr [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  prntError (hdr ++ ":")
  hSetSGR stderr [Reset]
  prntError $ unlines $ map ("  " ++) $ lines $ (prettyError bdy)
