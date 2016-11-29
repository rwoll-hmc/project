module AST where
data Command = Go | Stby | Warn deriving Eq
data Character = Character String deriving Eq
data Action = Enter | Exit deriving Eq
data Department = Department String deriving Eq
data Cue = Cue { department :: Department, number :: Int, command :: Command } deriving Eq
data CueGroup = CueGroup Marker [Cue] deriving (Eq, Show)
data Marker = Visual Character Action | Line Character String deriving Eq
data Scene = Scene Int [CueGroup] deriving (Eq, Show)
data Act = Act Int [Scene] deriving (Eq, Show)
data CueSheet = CueSheet { characters :: [Character], departments :: [Department], acts :: [Act] } deriving Eq

instance Show Command where
  show Go = "GO"
  show Stby = "STBY"
  show Warn = "WARN"

instance Show Character where
  show (Character ch) = '@':ch

instance Show Action where
  show Enter = "ENTR"
  show Exit = "EXIT"

instance Show Department where
  show (Department dp) = '#':dp

instance Show Cue where
  show (Cue dp i cmd) = show dp ++ " {" ++ show i ++ "} " ++ show cmd

instance Show Marker where
  show (Visual ch a) = "(visual) " ++ show ch ++ ":" ++ show a
  show (Line ch a) = "(line) " ++ show ch ++ ":" ++ show a

instance Show CueSheet where
  show (CueSheet cs ds as) =
    "Characters:\n" ++ (unlines $ map (\c -> "  " ++ show c) cs) ++ "\n" ++
    "Departments:\n" ++ (unlines $ map (\d -> "  " ++ show d) ds) ++ "\n" ++
    (concatMap showAct as) where

    showAct (Act i ss) = "Act " ++ show i ++ ":\n" ++ (concatMap showScene ss)
    showScene (Scene i gs) = "  Scene " ++ show i ++ ":\n" ++ (unlines $ map showCG gs)
    showCG (CueGroup m cs) = "    " ++ show m ++ "\n" ++ (unlines $ map (("      " ++) . show) cs)
