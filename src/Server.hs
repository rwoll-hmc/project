{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Aeson (ToJSON)
import SampleScript
import Web.Scotty
import qualified Text.Parsec as Parsec
import Parser (cueSheet)
import Interp
import Data.String.Conversions (cs)

main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do

        post "/process" $ do
          c <- body
          either handleErrors handleParsed (Parsec.parse cueSheet "i/o" $ cs c)

handleErrors e = error $ show e
handleParsed p = either handleErrors handleCompiled (transpile p)
handleCompiled c = either handleErrors handleSafe (checkUnkownCharacters theScript c) where
  handleSafe _ = either handleErrors json (eval theScript c)
