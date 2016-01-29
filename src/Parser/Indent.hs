module Parser.Indent
( module Text.Parsec
, Indent
, runIndent
, indented
, aligned
, withPos
) where

import Text.Parsec
import Text.Parsec.Pos

type Indent = Parsec String SourcePos

runIndent :: Indent a -> SourceName -> String -> Either ParseError a
runIndent ip name = runParser ip (initialPos name) name

indented :: Indent a -> Indent a
indented ip = do
  col1 <- sourceColumn <$> getPosition
  col2 <- sourceColumn <$> getState
  if col1 > col2
    then ip
    else fail "Indentation required"

aligned :: Indent a -> Indent a
aligned ip = do
  col1 <- sourceColumn <$> getPosition
  col2 <- sourceColumn <$> getState
  if col1 == col2
    then ip -- semicolon
    else fail "Incorrect indentation" -- vccurly

withPos :: Indent a -> Indent a
withPos ip = do -- vocurly
  st <- getState
  getPosition >>= putState
  ip <* putState st
