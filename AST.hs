module AST where
data Command = Go | Stby | Warn deriving (Eq, Show)
data Character = Character String deriving (Eq, Show)
data Action = Enter | Exit deriving (Eq, Show)
data Department = Department String deriving (Eq, Show)
data CueSheet = CueSheet { characters :: [Character], departments :: [Department], acts :: [Act] } deriving (Eq)
data Cue = Cue { department :: Department, number :: Int, command :: Command } deriving (Eq, Show)
data CueGroup = CueGroup Marker [Cue] deriving (Eq, Show)
data Marker = Visual Character Action | Line Character String deriving (Eq, Show)
data Scene = Scene Int [CueGroup] deriving (Eq, Show)
data Act = Act Int [Scene] deriving (Eq, Show)

instance Show CueSheet where
  show (CueSheet cs ds as) = (unlines $ map show cs) ++
                             (unlines $ map show ds) ++
                             (unlines $ map showAct as) where

    showAct (Act i ss) = "Act " ++ show i ++ "\n" ++ (unlines $ map showScene ss)
    showScene (Scene i gs) = "  Scene " ++ show i ++ "\n" ++ (unlines $ map showCG gs)
    showCG (CueGroup m cs) = "    " ++ show m ++ "\n" ++ (unlines $ map (("      " ++) . show) cs)
