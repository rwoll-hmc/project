module AST where
data Command = Go | Stby | Warn deriving (Eq, Show)
data Character = Character String deriving (Eq, Show)
data Department = Department String deriving (Eq, Show)
data CueSheet = CueSheet { characters :: [Character], departments :: [Department] } deriving (Eq, Show)
data Cue = Cue { department :: Department, number :: Int, command :: Command } deriving (Eq, Show)
data CueGroup = CueGroup [Cue] deriving (Eq, Show)
data CueGroupType = Visual | Line deriving (Eq, Show)
