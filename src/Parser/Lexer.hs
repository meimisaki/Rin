module Parser.Lexer where

import Common

import Data.Char
import Parser.Combinator
import Parser.Indent
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

symbol :: String -> Indent String
symbol = T.symbol haskell

lexeme :: Indent a -> Indent a
lexeme = T.lexeme haskell

whiteSpace :: Indent ()
whiteSpace = T.whiteSpace haskell

identifier :: Indent Name
identifier = T.identifier haskell

operator :: Indent Name
operator = T.operator haskell

reserved :: Name -> Indent ()
reserved = T.reserved haskell

reservedOp :: Name -> Indent ()
reservedOp = T.reservedOp haskell

charLit :: Indent Char
charLit = T.charLiteral haskell

stringLit :: Indent String
stringLit = T.stringLiteral haskell

natural :: Indent Integer
natural = T.natural haskell

float :: Indent Double
float = T.float haskell

naturalOrFloat :: Indent (Either Integer Double)
naturalOrFloat = T.naturalOrFloat haskell

comma :: Indent Char
comma = T.comma haskell *> return ','

dot :: Indent Char
dot = T.dot haskell *> return '.'

iparens_ :: Indent a -> Indent a
iparens_ ip = symbol "(" |> ip <| symbol ")"

ibrackets_ :: Indent a -> Indent a
ibrackets_ ip = symbol "[" |> ip <| symbol "]"

varId :: Indent Name
varId = try $ do
  var <- identifier
  if isLower (head var)
    then return var
    else fail ("Not a variable: " ++ var)

conId :: Indent Name
conId = try $ do
  con <- identifier
  if isUpper (head con)
    then return con
    else fail ("Not a constructor: " ++ con)

varOp :: Indent Name
varOp = try $ do
  op <- operator
  if head op /= ':'
    then return op
    else fail ("Not an ordinary operator: " ++ op)

conOp :: Indent Name
conOp = try $ do
  op <- operator
  if head op == ':'
    then return op
    else fail ("Not a constructor operator: " ++ op)

atomic :: Indent Name
atomic = identifier <|> try (iparens_ operator)

atomicVar :: Indent Name
atomicVar = varId <|> try (iparens_ varOp)

atomicCon :: Indent Name
atomicCon = conId <|> try (iparens_ conOp)

infixOp :: Indent Name
infixOp = operator <|> symbol "`" |> identifier <| symbol "`"

infixVarOp :: Indent Name
infixVarOp = varOp <|> symbol "`" |> varId <| symbol "`"

infixConOp :: Indent Name
infixConOp = conOp <|> symbol "`" |> conId <| symbol "`"
