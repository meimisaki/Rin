{
{-# LANGUAGE DeriveFunctor #-}

module Parser.Lexer
( Token (..), lexer
, P (..), PResult (..), PState (..), mkPState
, Loc (..), Located (..), unL
, popContext, pushCurrentContext
) where

import Common

import Control.Monad

import Data.Buffer
import Data.Char
import qualified Data.Map as M
}

$lower = [a-z]
$upper = [A-Z]
$digit = [0-9]
$idchar = [$lower $upper $digit \_\']
$symbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]

@varid = [$lower \_] $idchar*
@conid = $upper $idchar*
@varsym = ($symbol # \:) $symbol*
@consym = \: $symbol*
@number = $digit+

Rin :-

-- skip whitespaces
[\ \v\r\f]+ ;

\t { errorTab }

-- line comment
"--" [^$symbol] .* ;

-- TODO: nested comment, literals, etc.

<0> {
  @varid { varid }
  @conid { conid }
  @varsym { sym ITvarsym }
  @consym { sym ITconsym }
  @number { number }
  \{ { openBrace }
  \} { closeBrace }
  \[ { special ITobrack }
  \] { special ITcbrack }
  \( { special IToparen }
  \) { special ITcparen }
  \; { special ITsemi }
  \, { special ITcomma }
  \` { special ITbackquote }
  \n { begin bol }
}

<bol> {
  \n ;
  () { beginOfLine }
}

<layout> {
  \n ;
  \{ { hopefullyOpenBrace }
  () { newContext }
}

<ll> () { layoutLeft }

{
data Token
  -- keywords
  = ITdata
  | ITcase
  | ITof
  | ITif
  | ITthen
  | ITelse
  | ITlet
  | ITin
  | ITwhere
  | ITinfix
  | ITinfixl
  | ITinfixr
  | ITforall
  | ITunderscore
  -- reserved symbols
  | ITat
  | ITdot
  | ITddot
  | ITdcolon
  | ITequal
  | ITlambda
  | ITlarrow
  | ITrarrow
  | ITdarrow
  | ITvbar
  -- special symbols
  | ITocurly
  | ITccurly
  | ITvocurly
  | ITvccurly
  | ITobrack
  | ITcbrack
  | IToparen
  | ITcparen
  | ITsemi
  | ITcomma
  | ITbackquote
  -- identifiers
  | ITvarid Name
  | ITconid Name
  | ITvarsym Name
  | ITconsym Name
  -- literals
  | ITchar Char
  | ITstring String
  | ITnumber Int
  -- end of file
  | ITeof
  deriving (Show, Eq, Ord)

keywords :: M.Map Name Token
keywords = M.fromList
  [ ("data", ITdata)
  , ("case", ITcase)
  , ("of", ITof)
  , ("if", ITif)
  , ("then", ITthen)
  , ("else", ITelse)
  , ("let", ITlet)
  , ("in", ITin)
  , ("where", ITwhere)
  , ("infix", ITinfix)
  , ("infixl", ITinfixl)
  , ("infixr", ITinfixr)
  , ("forall", ITforall)
  , ("_", ITunderscore) ]

symbols :: M.Map Name Token
symbols = M.fromList
  [ ("@", ITat)
  , (".", ITdot) -- for `forall`
  , ("..", ITddot)
  , ("::", ITdcolon)
  , ("=", ITequal)
  , ("\\", ITlambda)
  , ("<-", ITlarrow)
  , ("->", ITrarrow)
  , ("=>", ITdarrow)
  , ("|", ITvbar) ]

type Action = Loc -> String -> P (Located Token)

errorTab :: Action
errorTab _ _ = fail "Invalid tab indentation"

varid :: Action
varid loc str = do
  when (tok `elem` [ITof, ITlet, ITwhere]) (pushLexState layout)
  return (L loc tok)
  where tok = case M.lookup str keywords of
          Nothing -> ITvarid str
          Just tok -> tok

conid :: Action
conid loc str = return (L loc (ITconid str))

sym :: (Name -> Token) -> Action
sym mk loc str = return (L loc tok)
  where tok = case M.lookup str symbols of
          Nothing -> mk str
          Just tok -> tok

number :: Action
number loc str = return (L loc n)
  where n = ITnumber (read str)

special :: Token -> Action
special tok loc _ = return (L loc tok)

begin :: Int -> Action
begin l _ _ = do
  pushLexState l
  lexToken

beginOfLine :: Action
beginOfLine loc _ = do
  pos <- getOffside
  case pos of
    LT -> do
      popContext
      return (L loc ITvccurly)
    EQ -> do
      popLexState
      return (L loc ITsemi)
    GT -> do
      popLexState
      lexToken

hopefullyOpenBrace :: Action
hopefullyOpenBrace loc str = do
  AI (Loc _ col) _ <- getInput
  ctx <- getContext
  let ok = case ctx of
        Layout n:_ -> n < col
        _ -> True
  if ok
    then do
      popLexState
      openBrace loc str
    else fail "Missing block"

newContext :: Action
newContext loc _ = do
  popLexState
  AI (Loc _ col) _ <- getInput
  ctx <- getContext
  case ctx of
    Layout n:_ | n >= col -> pushLexState ll
    _ -> setContext (Layout col:ctx)
  return (L loc ITvocurly)

layoutLeft :: Action
layoutLeft loc _ = do
  popLexState
  pushLexState bol
  return (L loc ITvccurly)

openBrace :: Action
openBrace loc _ = do
  ctx <- getContext
  setContext (NoLayout:ctx)
  return (L loc ITocurly)

closeBrace :: Action
closeBrace loc _ = do
  popContext
  return (L loc ITccurly)

newtype P a = P { unP :: PState -> PResult a }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = returnP
  (<*>) = ap

instance Monad P where
  (>>=) = thenP
  fail = failP

returnP :: a -> P a
returnP x = P $ \s ->
  POk s x

thenP :: P a -> (a -> P b) -> P b
P m `thenP` f = P $ \s -> case m s of
  POk s1 x -> unP (f x) s1
  PFailed loc err -> PFailed loc err

failP :: String -> P a
failP err = P $ \s@(PState { location = loc }) ->
  PFailed loc err

data PResult a
  = POk PState a
  | PFailed Loc String

data PState = PState
  { buffer :: Buffer
  , lastLoc :: Loc
  , location :: Loc
  , lexState :: [Int]
  , context :: [LContext] }

mkPState :: String -> PState
mkPState src = PState
  { buffer = mkBuffer src
  , lastLoc = loc
  , location = loc
  , lexState = [bol, 0]
  , context = [] }
  where loc = Loc 1 1

setLastLoc :: Loc -> P ()
setLastLoc loc = P $ \s ->
  POk s { lastLoc = loc } ()

pushLexState :: Int -> P ()
pushLexState l = P $ \s@(PState { lexState = ls }) ->
  POk s { lexState = l:ls } ()

popLexState :: P Int
popLexState = P $ \s@(PState { lexState = l:ls }) ->
  POk s { lexState = ls } l

getLexState :: P Int
getLexState = P $ \s@(PState { lexState = l:_ }) ->
  POk s l

data LContext
  = NoLayout
  | Layout Int
  deriving Show

getContext :: P [LContext]
getContext = P $ \s@(PState { context = ctx }) ->
  POk s ctx

setContext :: [LContext] -> P ()
setContext ctx = P $ \s ->
  POk s { context = ctx } ()

popContext :: P ()
popContext = P $ \s@(PState { lastLoc = loc, context = ctx }) -> case ctx of
  _:ctx1 -> POk s { context = ctx1 } ()
  _ -> PFailed loc "Layout context error"

pushCurrentContext :: P ()
pushCurrentContext = P $ \s@(PState { lastLoc = Loc _ col, context = ctx }) ->
  POk s { context = Layout col:ctx } ()

getOffside :: P Ordering
getOffside = P $ \s@(PState { lastLoc = Loc _ col, context = ctx }) -> 
  let ord = case ctx of
        Layout n:_ -> compare col n
        _ -> GT
  in POk s ord

data Loc = Loc Int Int
  deriving Show

advanceLoc :: Loc -> Char -> Loc
advanceLoc (Loc line col) ch
  | ch == '\n' = Loc (line + 1) 1
  | otherwise = Loc line (col + 1)

data Located a = L Loc a
  deriving (Show, Functor)

unL :: Located a -> a
unL (L _ x) = x

data AlexInput = AI Loc Buffer

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AI loc buf) = fmap next (nextByte buf)
  where next (byte, buf1) = (byte, AI loc1 buf1)
          where loc1 = advanceLoc loc ch
                ch = chr (fromIntegral byte)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = case prevByte buf of
  Nothing -> '\n'
  Just (byte, _) -> chr (fromIntegral byte)

getInput :: P AlexInput
getInput = P $ \s@(PState { location = loc, buffer = buf }) ->
  POk s (AI loc buf)

setInput :: AlexInput -> P ()
setInput (AI loc buf) = P $ \s ->
  POk s { location = loc, buffer = buf } ()

lexToken :: P (Located Token)
lexToken = do
  inp@(AI loc buf) <- getInput
  sc <- getLexState
  case alexScan inp sc of
    AlexEOF -> do
      setLastLoc loc
      return (L loc ITeof)
    AlexError _ -> fail "Lexical error"
    AlexSkip inp1 _ -> do
      setInput inp1
      lexToken
    AlexToken inp1@(AI loc1 buf1) len act -> do
      setInput inp1
      setLastLoc loc
      act loc (lexeme buf len)

lexer :: (Located Token -> P a) -> P a
lexer cont = lexToken >>= cont
}
