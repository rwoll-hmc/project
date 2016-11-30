{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Base.Class
import AST
import qualified Data.Set as Set
import Control.Monad

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
formatMarker (PromptMarker (Visual (Character c) a) []) = do
  center $ textit $ fromString (c ++ " : " ++ show a)
formatMarker (PromptMarker (Visual (Character c) a) cs) = do
  center (addCueNotes cs <> comm1 "hl" (textit $ fromString (c ++ " : " ++ show a)))
formatMarker (PromptMarker (Line (Character c) l) []) = do
  flushleft (textbf (fromString (c ++ ": ")) <> fromString l)
formatMarker (PromptMarker (Line (Character c) l) cs) = do
  flushleft (addCueNotes cs <> comm1 "hl" (textbf( fromString (c++": ")) <> fromString l))

addCueNotes :: Monad m => [Cue] -> LaTeXT_ m
addCueNotes [] = do ""
addCueNotes cs = do
  raw "\\marginnote{"
  mapM_ (\c -> (texttt $ fromString $ show c ++ " TODO") <> newline) cs
  raw "}"

-- Sample Script With Markup Injected
theScript :: PromptScript
theScript = PromptScript
  "DSLs: A Play"
  (Set.fromList [Character "ROSS", Character "PROF"])
  (Set.fromList [Department "LX", Department "SD"])
  (replicate 5 a)

a = PromptAct 0 $ replicate 2 $ PromptScene 47 [
    PromptMarker (Visual (Character "CECIL") Enter) $ [Cue (Department "LX") 10 Stby],
    PromptMarker (Line (Character "CECIL") "Hello, world! This is great and it is a really really long sentance that is so long it will probably have to wrap around the page in an infinite loop forever and ever and ever.") $ [Cue (Department "LX") 10 Stby, Cue (Department "SD") 20 Warn],
    PromptMarker (Line (Character "CECIL") "Hello, world! This is great and it is a really really long sentance that is so long it will probably have to wrap around the page in an infinite loop forever and ever and ever.") [],
    PromptMarker (Line (Character "CECIL") "Hello, world! This is great and it is a really really long sentance that is so long it will probably have to wrap around the page in an infinite loop forever and ever and ever.") $ [Cue (Department "LX") 10 Stby, Cue (Department "SD") 20 Warn],
    PromptMarker (Visual (Character "CECIL") Enter) []
  ]
