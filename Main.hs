module Main where

import qualified Parser
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  c <- getContents
  putStrLn $ show $ Parsec.parse Parser.cueSheet "stdin" c
