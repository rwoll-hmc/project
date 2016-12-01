module AST where

import qualified Data.Set as Set

data Command = Go
             | Stby
             | Warn
  deriving Eq

data Character = Character String
  deriving Eq

data Action = Enter
            | Exit
  deriving Eq

data Department = Department String
  deriving Eq

data Cue = Cue { department :: Department, number :: Int, command :: Command }
  deriving Eq

data CueGroup = CueGroup { cgMarker :: Marker, cgCues :: [Cue] }
  deriving (Eq, Show)

data Marker = Visual Character Action (Maybe Int)
            | Line Character String (Maybe Int)
  deriving Eq

data CueScene = CueScene { csIndex :: Int, csGroups :: [CueGroup] }
  deriving (Eq, Show)

data CueAct = CueAct { cueActIdx :: Int, cueActScenes :: [CueScene] }
  deriving (Eq, Show)

data CueSheet =
       CueSheet
         { characters  :: [Character]
         , departments :: [Department]
         , acts        :: [CueAct]
         }
  deriving Eq

data PromptScript =
       PromptScript
         { pTitle       :: String
         , pCharacters  :: Set.Set Character
         , pDepartments :: Set.Set Department
         , pActs        :: [PromptAct]
         }
  deriving (Eq, Show)

data PromptAct = PromptAct { pActId :: Int, pActScenes :: [PromptScene] }
  deriving (Eq, Show)

data PromptScene = PromptScene { pSceneId :: Int, pMarkers :: [PromptMarker] }
  deriving (Eq, Show)

data PromptMarker = PromptMarker { pMarker :: Marker, pCues :: [Cue] }
  deriving (Eq, Show)

instance Show Command where
  show Go = "GO"
  show Stby = "STBY"
  show Warn = "WARN"

instance Show Character where
  show (Character ch) = '@' : ch

instance Show Action where
  show Enter = "ENTR"
  show Exit = "EXIT"

instance Show Department where
  show (Department dp) = '#' : dp

instance Show Cue where
  show (Cue dp i cmd) = show dp ++ " {" ++ show i ++ "} " ++ show cmd

showDisamb (Just i) = "(" ++ show i ++ ")"
showDisamb Nothing = "(-)"

instance Show Marker where
  show (Visual ch a i) = "(visual) " ++ show ch ++ ":" ++ show a ++ " " ++ showDisamb i
  show (Line ch a i) = "(line) " ++ show ch ++ ":" ++ show a ++ " " ++ showDisamb i

instance Show CueSheet where
  show (CueSheet cs ds as) =
    "Characters:\n" ++
    (unlines $ map (\c -> "  " ++ show c) cs) ++
    "\n" ++
    "Departments:\n" ++
    (unlines $ map (\d -> "  " ++ show d) ds) ++
    "\n" ++
    (concatMap showAct as)
    where
      showAct (CueAct i ss) = "Act " ++ show i ++ ":\n" ++ (concatMap showScene ss)
      showScene (CueScene i gs) = "  Scene " ++ show i ++ ":\n" ++ (unlines $ map showCG gs)
      showCG (CueGroup m cs) = "    " ++ show m ++ "\n" ++ (unlines $ map (("      " ++) . show) cs)

instance Ord Department where
  compare (Department a) (Department b) = compare a b

instance Ord Character where
  compare (Character a) (Character b) = compare a b
