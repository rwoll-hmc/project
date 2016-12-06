-- | Common errors and utilities.
module Errors where

  import AST
  import qualified Text.Parsec as Parsec

  -- | Possible program errors.
  data Error = NoMatchError PromptMarker
             | AmbiguousError PromptMarker [PromptMarker]
             | DuplicateCharacterDeclaration Character
             | DuplicateDepartmentDeclaration Department
             | UndeclaredCharacterError Character
             | UndeclaredDepartmentError Department
             | UnkownTargetCharacterError Character
             | OutOfOrderOrDuplicateActError CueAct
             | OutOfOrderOrDuplicateSceneError CueScene
    deriving (Eq, Show)

  -- | Pretty Printing Class
  class PrettyError a where
    -- | Return a string that has a verbose error message formatted with newlines.
    prettyError :: a -> String

  -- | Pretty printer for a list of `PrettyError` instances.
  instance PrettyError a => PrettyError [a] where
    prettyError = foldr (\l acc -> prettyError l ++ "\n" ++ acc) ""

  -- | Pretty print an error message in a verbose, helpful way.
  instance PrettyError Error where
    prettyError (NoMatchError (PromptMarker m _)) = unlines
      [
        "[NoMatchError] The following marker could not be placed:",
        "",
        "  " ++ show m,
        "",
        "Perhaps you spelled something incorrectly or have used the wrong index."
      ]
    prettyError (AmbiguousError m ms) = unlines
      [
        "[AmbiguousError] The following marker has too many possible options:",
        "",
        "  " ++ show m,
        "",
        "Here are all the possible matches:",
        "",
        "  " ++ foldr (\l acc -> show l ++ "\n  " ++ acc) "" ms,
        "",
        "Please disambiguate the placement by providing an index or specifying more of a line:",
        "      (visual) @CECIL:ENTR (99)",
        "   OR",
        "      (line) @CECIL:\"more words here\""
      ]
    prettyError (DuplicateCharacterDeclaration c) = unlines
      [
        "[DuplicateCharacterDeclaration] The following character is defined twice:",
        "",
        "  " ++ show c,
        "",
        "Delete the extra occurences in your 'Characters' declaration block."
      ]
    prettyError (DuplicateDepartmentDeclaration d) = unlines
      [
        "[DuplicateDepartmentDeclaration] The following character is defined twice:",
        "",
        "  " ++ show d,
        "",
        "Delete the extra occurences in your 'Departments' declaration block."
      ]
    prettyError (UndeclaredCharacterError c) = unlines
      [
        "[UndeclaredCharacterError] The following character was referenced in a cue, but not defined:",
        "",
        "  " ++ show c,
        "",
        "Perhaps it is a typo? If not, please declare the character in the 'Characters' block."
      ]
    prettyError (UndeclaredDepartmentError c) = unlines
      [
        "[UndeclaredDepartmentError] The following department was referenced in a cue, but not defined:",
        "",
        "  " ++ show c,
        "",
        "Perhaps it is a typo? If not, please declare the department in the 'Departments' block."
      ]
    prettyError (UnkownTargetCharacterError c) = unlines
      [
        "[UnkownTargetCharacterError] The following character was referenced in a the cuesheet,",
        "                             but does not appear in the target script:",
        "",
        "  " ++ show c,
        "",
        "Perhaps it is a typo? If so, please change the character declaration and all references."
      ]
    prettyError (OutOfOrderOrDuplicateActError (CueAct i _)) = unlines
      [
        "[OutOfOrderOrDuplicateActError] The following act was declared out of order or twice:",
        "",
        "  Act " ++ show i,
        "",
        "Please check the numbering."
      ]
    prettyError (OutOfOrderOrDuplicateSceneError (CueScene i _)) = unlines
      [
        "[OutOfOrderOrDuplicateSceneError] The following scene was declared out of order or twice:",
        "",
        "  Scene " ++ show i,
        "",
        "Please check the numbering."
      ]

  -- | Pretty print parsec error.
  instance PrettyError Parsec.ParseError where
    prettyError = show
