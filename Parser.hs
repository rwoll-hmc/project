{-# LANGUAGE FlexibleContexts #-}

module Parser where

import qualified Text.Parsec as Parsec
import qualified AST
import Control.Applicative

baseIndent :: Int
baseIndent = 2

indentCalc :: Int -> String
indentCalc lvl = replicate (lvl * baseIndent) ' '

indent lvl p = Parsec.try $ (Parsec.string (indentCalc lvl) *> p) <* (Parsec.many (Parsec.char ' ') *> Parsec.newline)

character :: Parsec.Parsec String () AST.Character
character = AST.Character <$> (Parsec.char '@' *> Parsec.many1 Parsec.upper)

characters :: Parsec.Parsec String () [AST.Character]
characters = indent 0 (Parsec.string "Characters:") *> Parsec.many1 (indent 1 character)

department :: Parsec.Parsec String () AST.Department
department = AST.Department <$> (Parsec.char '#' *> Parsec.many1 Parsec.upper)

departments :: Parsec.Parsec String () [AST.Department]
departments = indent 0 (Parsec.string "Departments:") *> Parsec.many1 (indent 1 department)

command :: Parsec.Parsec String () AST.Command
command =  Parsec.string "GO" *> pure AST.Go
       <|> Parsec.string "STBY" *> pure AST.Stby
       <|> Parsec.string "WARN" *> pure AST.Warn

cueNumber :: Parsec.Parsec String () Int
cueNumber = Parsec.char '{' *> (read <$> Parsec.many1 Parsec.digit) <* Parsec.char '}'

cue :: Parsec.Parsec String () AST.Cue
cue = indent 3 (AST.Cue <$> (department <* Parsec.char ' ') <*> (cueNumber <* Parsec.char ' ') <*> command)

action :: Parsec.Parsec String () AST.Action
action =  Parsec.string "ENTR" *> pure AST.Enter
      <|> Parsec.string "EXIT" *> pure AST.Exit

parens p = Parsec.char '(' *> p <* Parsec.char ')'

quotedString :: Parsec.Parsec String () String
quotedString = Parsec.char '"' *> Parsec.many (Parsec.oneOf " <>?/.,:;'|\\{}[]~!@#$%^&*()_+=-`" <|> Parsec.alphaNum ) <* Parsec.char '"'

visualCueGroupMarker :: Parsec.Parsec String () AST.Marker
visualCueGroupMarker = AST.Visual <$> (parens (Parsec.string "visual") *> Parsec.char ' ' *> character <* Parsec.char ':') <*> action

lineCueGroupMarker :: Parsec.Parsec String () AST.Marker
lineCueGroupMarker = AST.Line <$> (parens (Parsec.string "line") *> Parsec.char ' ' *> character <* Parsec.char ':') <*> quotedString

cueGroup :: Parsec.Parsec String () AST.CueGroup
cueGroup = AST.CueGroup <$> indent 2 (visualCueGroupMarker) <*> Parsec.many1 cue <* Parsec.newline

scene :: Parsec.Parsec String () AST.Scene
scene = AST.Scene <$> indent 1 (Parsec.string "Scene " *> (read <$> Parsec.many1 Parsec.digit) <* Parsec.char ':')
                  <*> Parsec.many1 cueGroup

act :: Parsec.Parsec String () AST.Act
act = AST.Act <$> indent 0 (Parsec.string "Act " *> (read <$> Parsec.many1 Parsec.digit) <* Parsec.char ':')
              <*> Parsec.many1 scene

cueSheet :: Parsec.Parsec String () AST.CueSheet
cueSheet = AST.CueSheet
            <$> (characters <* Parsec.newline)
            <*> (departments <* Parsec.newline)
            <*> (Parsec.many1 act <* indent 0 (Parsec.string "--- END ---"))
