module AST where

data Character = Character String deriving (Eq, Show)
data Department = Department String deriving (Eq, Show)
data CueSheet = CueSheet { characters :: [Character], departments :: [Department] } deriving (Eq, Show)
