{-# LANGUAGE FlexibleContexts #-}

-- | DSL parser.
module Parser where

import qualified AST
import           Control.Applicative
import qualified Text.Parsec         as Parsec

-- | Indent multiplier for our langauge.
baseIndent :: Int
baseIndent = 2

-- | Indent calculator.
indentCalc :: Int    -- ^ Indent level.
           -> String -- ^ String representing that indent.
indentCalc lvl = replicate (lvl * baseIndent) ' '

-- | Indented parser wrapper.
indent lvl p = Parsec.try $ (Parsec.string (indentCalc lvl) *> p)
                            <* (Parsec.many (Parsec.char ' ') *> Parsec.newline)

-- | Parens parser wrapper.
parens p = Parsec.char '(' *> p <* Parsec.char ')'

-- | Parse a character name. (e.g. `@ME`)
character :: Parsec.Parsec String () AST.Character
character = AST.Character <$> (Parsec.char '@' *> Parsec.many1 (Parsec.upper <|> Parsec.char '_'))

-- | Parse a block of characters.
characters :: Parsec.Parsec String () [AST.Character]
characters = indent 0 (Parsec.string "Characters:") *> Parsec.many1 (indent 1 character)

-- | Parse a department. (e.g. `#LX`)
department :: Parsec.Parsec String () AST.Department
department = AST.Department <$> (Parsec.char '#' *> Parsec.many1 Parsec.upper)

-- | Parse a block of departments.
departments :: Parsec.Parsec String () [AST.Department]
departments = indent 0 (Parsec.string "Departments:") *> Parsec.many1 (indent 1 department)

-- | Parse a command.
command :: Parsec.Parsec String () AST.Command
command = Parsec.string "GO" *> pure AST.Go
          <|> Parsec.string "STBY" *> pure AST.Stby
          <|> Parsec.string "WARN" *> pure AST.Warn

-- | Parse a formatted cue number (e.g. `{dd...dd}`)
cueNumber :: Parsec.Parsec String () Int
cueNumber = Parsec.char '{' *> (int) <* Parsec.char '}'

-- | Parse a cue. (e.g. `#LX {10} STBY "Optional comment..."`)
cue :: Parsec.Parsec String () AST.Cue
cue = indent 3 $
  AST.Cue <$> (department <* Parsec.char ' ')
          <*> (cueNumber <* Parsec.char ' ')
          <*> command
          <*> Parsec.optionMaybe (Parsec.char ' ' *> quotedString)

-- | Parse an action.
action :: Parsec.Parsec String () AST.Action
action = Parsec.try (Parsec.string "ENTR" *> pure AST.Enter)
         <|> Parsec.string "EXIT" *> pure AST.Exit

-- | Parse a quoted string (with some limited characters)
quotedString :: Parsec.Parsec String () String
quotedString = quote *>
               Parsec.many (Parsec.oneOf allowedExtras <|> Parsec.alphaNum)
               <* quote
  where
    allowedExtras = " <>?/.,:;'|\\{}[]~!@#$%^&*()_+=-`"
    quote = Parsec.char '"'

-- | Parse a visual cue group marker.
-- @
--     (visual) @HELLO:ENTR (2)
-- @
visualCueGroupMarker :: Parsec.Parsec String () AST.Marker
visualCueGroupMarker = Parsec.try $ AST.Visual <$> (parens (Parsec.string "visual")
                                                    *> Parsec.char ' '
                                                    *> character
                                                    <* Parsec.char ':')
                                               <*> action
                                               <*> Parsec.optionMaybe
                                                     (Parsec.char ' ' *> parens (int))

-- | Parse a line cue group marker.
-- @
--     (line) @HELLO:"Hello, World!" (2)
-- @
lineCueGroupMarker :: Parsec.Parsec String () AST.Marker
lineCueGroupMarker =
  Parsec.try $ AST.Line <$> (parens (Parsec.string "line")
                             *> Parsec.char ' ' *> character <* Parsec.char ':')
                        <*> quotedString
                        <*> Parsec.optionMaybe (Parsec.char ' ' *> parens (int))

-- | Parse a cue group.
cueGroup :: Parsec.Parsec String () AST.CueGroup
cueGroup = Parsec.try
             (AST.CueGroup <$> indent 2 (Parsec.try visualCueGroupMarker <|> lineCueGroupMarker)
                           <*> Parsec.many1 cue <* Parsec.newline)

-- | Parse a scene.
scene :: Parsec.Parsec String () AST.CueScene
scene = Parsec.try
          (AST.CueScene <$> indent 1 (Parsec.string "Scene " *> (int) <* Parsec.char ':')
                        <*> Parsec.many1 cueGroup)

-- | Parse an act.
act :: Parsec.Parsec String () AST.CueAct
act = AST.CueAct <$> indent 0 (Parsec.string "Act " *> (int) <* Parsec.char ':')
                 <*> Parsec.many1 scene

-- | Parse an entire cue sheet.
cueSheet :: Parsec.Parsec String () AST.CueSheet
cueSheet = AST.CueSheet <$> (characters <* Parsec.newline)
                        <*> (departments <* Parsec.newline)
                        <*> (Parsec.many1 act
                             <* indent 0 (Parsec.string "--- END ---"))

-- | Parse an integer.
int :: Parsec.Parsec String () Int
int = read <$> Parsec.many1 Parsec.digit
