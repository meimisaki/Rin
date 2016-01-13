module Parser.Indent
( module Text.Parsec
, Indent
, runIndent
, inline
, indented
, aligned
, withPos
) where

import Text.Parsec
import Text.Parsec.Pos

type Indent = Parsec String SourcePos

runIndent :: Indent a -> SourceName -> String -> Either ParseError a
runIndent p s = runParser p (initialPos s) s

inline :: Indent a -> Indent a
inline p = do
  l1 <- sourceLine <$> getPosition
  l2 <- sourceLine <$> getState
  if l1 == l2
    then p
    else fail "Over one line"

indented :: Indent a -> Indent a
indented p = do
  c1 <- sourceColumn <$> getPosition
  c2 <- sourceColumn <$> getState
  if c1 > c2
    then p
    else fail "Indentation required"

aligned :: Indent a -> Indent a
aligned p = do
  c1 <- sourceColumn <$> getPosition
  c2 <- sourceColumn <$> getState
  if c1 == c2
    then p -- semicolon
    else fail "Incorrect indentation" -- vccurly

withPos :: Indent a -> Indent a
withPos p = do -- vocurly
  s <- getState
  getPosition >>= putState
  p <* putState s
