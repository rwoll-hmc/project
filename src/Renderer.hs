{-# LANGUAGE OverloadedStrings #-}

-- | LaTeX rendering utilities.
module Renderer where

import           AST
import           Control.Monad
import           Data.Map.Strict       (toList)
import qualified Data.Set              as Set
import           Text.LaTeX
import           Text.LaTeX.Base.Class

-- | Render an IR `PromptScript` into a LaTeX file.
main :: PromptScript -> IO ()
main theScript = execLaTeXT simple >>= renderFile "script.tex"
  where
    simple :: Monad m => LaTeXT_ m
    simple = do
      thePreamble
      document theBody

    thePreamble :: Monad m => LaTeXT_ m
    thePreamble = do
      -- error "ARGH"
      documentclass [] article
      usepackage
        [ "top=1.5cm"
        , "bottom=1.5cm"
        , "outer=10cm"
        , "inner=2cm"
        , "heightrounded"
        , "marginparwidth=8cm"
        , "marginparsep=1cm"
        ]
        "geometry"
      usepackage [] "marginnote"
      usepackage [] "soul"
      usepackage [] "color"
      comm1 "sethlcolor" "yellow"
      title (fromString $ pTitle theScript)

    theBody :: Monad m => LaTeXT_ m
    theBody = do
      center $ textbf $ large3 $ fromString $ pTitle theScript
      formatScript theScript

    formatScript :: Monad m => PromptScript -> LaTeXT_ m
    formatScript script = do
      mapM_ formatAct (toList $ pActs script)

    formatAct :: Monad m => (Int, PromptAct) -> LaTeXT_ m
    formatAct (i, act) = do
      center $ large2 $ textbf $ fromString $ ("ACT " ++ show i)
      mapM_ formatScene (toList $ pActScenes act)

    formatScene :: Monad m => (Int, PromptScene) -> LaTeXT_ m
    formatScene (i, scene) = do
      center $ large $ textbf $ fromString $ ("Scene " ++ show i)
      mapM_ formatMarker (pMarkers scene)

    formatMarker :: Monad m => PromptMarker -> LaTeXT_ m
    formatMarker (PromptMarker (Visual (Character c) a _) []) = do
      center $ textit $ fromString (c ++ " : " ++ show a)
    formatMarker (PromptMarker (Visual (Character c) a _) cs) = do
      center (addCueNotes cs <> comm1 "hl" (textit $ fromString (c ++ " : " ++ show a)))
    formatMarker (PromptMarker (Line (Character c) l _) []) = do
      flushleft (textbf (fromString (c ++ ": ")) <> fromString l)
    formatMarker (PromptMarker (Line (Character c) l _) cs) = do
      flushleft (addCueNotes cs <> comm1 "hl" (textbf (fromString (c ++ ": ")) <> fromString l))

    addCueNotes :: Monad m => [Cue] -> LaTeXT_ m
    addCueNotes [] = do
      ""
    addCueNotes cs = do
      raw "\\marginnote{"
      mapM_ prettyCue cs
      raw "}"

    prettyCue :: Monad m => Cue -> LaTeXT_ m
    prettyCue (Cue d i cmd cmt) = do
      (texttt
         ((fromString $ show d)
          <> (fromString $ " {" ++ show i ++ "} ")
          <> (comm1 "color" $ selColor cmd)
          <> (fromString $ show cmd)))
      " "
      textit $ fromString $ maybe "" id cmt
      newline

      where
        selColor Go = "green"
        selColor Stby = "blue"
        selColor Warn = "yellow"
