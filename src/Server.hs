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
import Network.Wai.Middleware.Static
import Errors
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)


main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        middleware $ staticPolicy (noDots >-> addBase "build")

        post "/process" $ do
          c <- body
          either handleErrors handleParsed (Parsec.parse cueSheet "i/o" $ cs c)

handleErrors e = do
  json $ ErrorResponse $ cs $ prettyError e

handleParsed p = either handleErrors handleCompiled (transpile p)
handleCompiled c = either handleErrors handleSafe (checkUnkownCharacters theScript c) where
  handleSafe _ = either handleErrors json (eval theScript c)

data ErrorResponse = ErrorResponse { errors :: String } deriving (Eq, Show, Generic)

instance ToJSON ErrorResponse
