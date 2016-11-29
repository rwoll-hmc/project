module Main where

import qualified Parser
import qualified Text.Parsec as Parsec
import System.Exit

main :: IO ()
main = do
  c <- getContents
  case Parsec.parse Parser.cueSheet "stdin" c of
    (Left e) -> putStrLn ("Error while parsing:\n" ++ show e) >> exitFailure
    (Right r) -> putStrLn $ show r ++ "--- END ---"
