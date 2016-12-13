{-# LANGUAGE DeriveGeneric #-}

-- | `AST` encodes the different datatypes used throughout the application.
module AST where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

-- | Encoding of theater keywords for intruction of operators.
data Command = Go   -- ^ Instructs operator should take instruction immediately.
             | Stby -- ^ Instructs operator to get ready and confirm ready.
             | Warn -- ^ A polite/stategic reminder for some instruction.
  deriving (Eq, Generic)

-- | An encoding of a physical `Character` that could appear in a script, cue, etc.
data Character = Character { charName :: String }
  deriving (Eq, Generic)

-- | An encoding of a physical action that can be taken.
data Action = Enter
            | Exit
  deriving (Eq, Generic)

-- | A `Department` corresponds to a physical department in the theater.
--   (e.g. Lights, Sound, Backstage, etc.)
data Department = Department { deptName :: String }
  deriving (Eq, Generic)

-- | A `Cue` is the smallest unit of action in our representation. This representation
--   has no notion of time/placement by design and must be coupled else where (e.g `CueGroup`
--   which has a `Marker` specifying time.)
data Cue =
       Cue
         { department :: Department
         , number     :: Int
         , command    :: Command
         , comment    :: Maybe String
         }
  deriving (Eq, Generic)

-- | A binding of a `Marker` and list of cues to occur at that `Marker`.
data CueGroup = CueGroup { cgMarker :: Marker, cgCues :: [Cue] }
  deriving (Eq, Show)

-- | A `Marker` is a (possibly partial) reference to a part of a play.
data Marker = Visual { mChar :: Character, mAction :: Action, mIdx :: (Maybe Int) }
            | Line { mChar :: Character, mLine :: String, mIdx :: (Maybe Int) }
  deriving (Eq, Generic)

-- | Internal representation of a scene and any groups of cues that appear
--   throughout it.
data CueScene = CueScene { csIndex :: Int, csGroups :: [CueGroup] }
  deriving (Eq, Show)

-- | Internal represenation of an act and its scenes.
data CueAct = CueAct { cueActIdx :: Int, cueActScenes :: [CueScene] }
  deriving (Eq, Show)

-- | Internal representation of a cue sheet: characters, departments, and acts.
data CueSheet =
       CueSheet
         { characters  :: [Character]
         , departments :: [Department]
         , acts        :: [CueAct]
         }
  deriving Eq

-- | Analagous to `CueSheet` for use in `Prompt*` using a `Map` internally
--   for easier traversals, lookups, and guarentees uniqueness as well as `Set`s.
data PromptScript =
       PromptScript
         { pTitle       :: String
         , pCharacters  :: Set.Set Character
         , pDepartments :: Set.Set Department
         , pActs        :: Map.Map Int PromptAct
         }
  deriving (Eq, Show, Generic)

-- | Analagous to `CueAct` for use in `Prompt*` using a `Map` internally
--   for easier traversals, lookups, and guarentees uniqueness.
data PromptAct = PromptAct { pActScenes :: Map.Map Int PromptScene }
  deriving (Eq, Show, Generic)

-- | Analagous to `CueScene` for use in `Prompt*` using a `Map` internally
--   for easier traversals, lookups, and guarentees of uniqueness.
data PromptScene = PromptScene { pMarkers :: Map.Map Int PromptMarker }
  deriving (Eq, Show, Generic)

-- | Analogous to `CueMarker` for use in `Prompt*`.
data PromptMarker = PromptMarker { pMarker :: Marker, pCues :: [Cue] }
  deriving (Eq, Show, Generic)

-- | Pretty print `Commands` in standard theater notation.
instance Show Command where
  show Go = "GO"
  show Stby = "STBY"
  show Warn = "WARN"

-- | Pretty print `Character` names in DSL format.
instance Show Character where
  show (Character ch) = '@' : ch

-- | Pretty print `Action`s in standard theater notation.
instance Show Action where
  show Enter = "ENTR"
  show Exit = "EXIT"

-- | Pretty print department names in DSL format.
instance Show Department where
  show (Department dp) = '#' : dp

-- | Pretty print `Cue`s in DSL format.
instance Show Cue where
  show (Cue dp i cmd cmt) = show dp ++ " {" ++ show i ++ "} " ++ show cmd ++ showCmt cmt
    where
      showCmt = maybe "" (\c -> " \"" ++ c ++ "\"")

-- | Pretty print 'Marker' in DSL format.
instance Show Marker where
  show m = (case m of
              Visual ch a i -> "(visual) " ++ show ch ++ ":" ++ show a ++ showDisamb i
              Line ch a i   -> "(line) " ++ show ch ++ ":" ++ show a ++ showDisamb i)
    where
      showDisamb = maybe "" (\i -> " (" ++ show i ++ ")")

-- | Pretty print CueSheet in DSL format.
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

-- | Create an ordering of `Department`s based on `String`'s `Ord`.
instance Ord Department where
  compare (Department a) (Department b) = compare a b

-- | Create an ordering of `Character`s based on `String`'s `Ord`.
instance Ord Character where
  compare (Character a) (Character b) = compare a b

instance ToJSON PromptScript
instance ToJSON PromptAct
instance ToJSON PromptScene
instance ToJSON PromptMarker
instance ToJSON Department
instance ToJSON Marker
instance ToJSON Character
instance ToJSON Cue
instance ToJSON Action
instance ToJSON Command
