{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Base.Class
import AST
import qualified Data.Set as Set
import Control.Monad
import SampleScript

main :: IO ()
main = execLaTeXT simple >>= renderFile "script.tex"

simple :: Monad m => LaTeXT_ m
simple = do
 thePreamble
 document theBody

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
 documentclass [] article
 usepackage [
  "top=1.5cm",
  "bottom=1.5cm",
  "outer=10cm",
  "inner=2cm",
  "heightrounded",
  "marginparwidth=8cm",
  "marginparsep=1cm"] "geometry"
 usepackage [] "marginnote"
 usepackage [] "soul"
 usepackage [] "color"
 comm1 "sethlcolor" "yellow"
 title (fromString $ pTitle theScript)

theBody :: Monad m => LaTeXT_ m
theBody = do
 center $ textbf $ large3 $ fromString $ pTitle theScript -- add Title
 formatScript theScript

formatScript :: Monad m => PromptScript -> LaTeXT_ m
formatScript script = do
  mapM_ formatAct (pActs script)

formatAct :: Monad m => PromptAct -> LaTeXT_ m
formatAct act = do
  center $ large2 $ textbf $ fromString $ ("ACT " ++ show (pActId act))
  mapM_ formatScene (pActScenes act)

formatScene :: Monad m => PromptScene -> LaTeXT_ m
formatScene scene = do
  center $ large $ textbf $ fromString $ ("Scene " ++ show (pSceneId scene))
  mapM_ formatMarker (pMarkers scene)

formatMarker :: Monad m => PromptMarker -> LaTeXT_ m
formatMarker (PromptMarker (Visual (Character c) a _) []) = do
  center $ textit $ fromString (c ++ " : " ++ show a)
formatMarker (PromptMarker (Visual (Character c) a _) cs) = do
  center (addCueNotes cs <> comm1 "hl" (textit $ fromString (c ++ " : " ++ show a)))
formatMarker (PromptMarker (Line (Character c) l _) []) = do
  flushleft (textbf (fromString (c ++ ": ")) <> fromString l)
formatMarker (PromptMarker (Line (Character c) l _) cs) = do
  flushleft (addCueNotes cs <> comm1 "hl" (textbf( fromString (c++": ")) <> fromString l))

addCueNotes :: Monad m => [Cue] -> LaTeXT_ m
addCueNotes [] = do ""
addCueNotes cs = do
  raw "\\marginnote{"
  mapM_ (\c -> (texttt $ fromString $ show c ++ " TODO") <> newline) cs
  raw "}"
