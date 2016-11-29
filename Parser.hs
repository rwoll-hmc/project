{-# LANGUAGE FlexibleContexts #-}

module Parser where

import qualified Text.Parsec as Parsec
import qualified AST
import Control.Applicative

baseIndent :: Int
baseIndent = 2

indentCalc :: Int -> String
indentCalc lvl = replicate (lvl * baseIndent) ' '

indent lvl p = (Parsec.string (indentCalc lvl) *> p) <* (many (Parsec.char ' ') *> Parsec.newline)

character :: Parsec.Parsec String () AST.Character
character = AST.Character <$> (Parsec.char '@' *> some Parsec.upper)

characters :: Parsec.Parsec String () [AST.Character]
characters = indent 0 (Parsec.string "Characters:") *> some (indent 1 character)

department :: Parsec.Parsec String () AST.Department
department = AST.Department <$> (Parsec.char '#' *> some Parsec.upper)

departments :: Parsec.Parsec String () [AST.Department]
departments = indent 0 (Parsec.string "Departments:") *> some (indent 1 department)

command :: Parsec.Parsec String () AST.Command
command =  Parsec.string "GO" *> pure AST.Go
       <|> Parsec.string "STBY" *> pure AST.Stby
       <|> Parsec.string "WARN" *> pure AST.Warn

cuenumber :: Parsec.Parsec String () Int
cuenumber = Parsec.char '{' *> (read <$> some Parsec.digit) <* Parsec.char '}'

cue :: Parsec.Parsec String () AST.Cue
cue = AST.Cue <$> (department <* Parsec.char ' ') <*> (cuenumber <* Parsec.char ' ') <*> command

cuesheet :: Parsec.Parsec String () AST.CueSheet
cuesheet = AST.CueSheet <$> (characters <* Parsec.spaces) <*> departments
