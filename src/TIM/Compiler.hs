module TIM.Compiler
( compile
) where

import Common

import Core
import qualified Data.Heap as H
import qualified Data.Map as M
import TIM.Types

compile :: Program Name -> TIM
compile prog = TIM
  { instrs = [Enter (Label "main")]
  , framePtr = FrameNull
  , stack = initialStack
  , valueStack = initialValueStack
  , dump = initialDump
  , heap = H.empty
  , codeStore = M.union compiledCode compiledPrim
  , stats = initialStats }
  where scDefs = preludeDefs ++ prog
        compiledCode = M.fromList (map (compileSC initialEnv) scDefs)
        initialEnv = M.fromList [(name, Label name) | name <- names]
        names = [name | (name, _, _) <- scDefs] ++ M.keys compiledPrim

initialStack :: Stack
initialStack = [([], FrameNull)]

initialValueStack :: ValueStack
initialValueStack = []

initialDump :: Dump
initialDump = ()

operators :: [String]
operators = ["+", "-", "*", "/", ">", ">=", "<", "<=", "==", "/="]

mkOp :: String -> Op
mkOp op = case op of
  "+" -> Add
  "-" -> Sub
  "*" -> Mult
  "/" -> Div
  ">" -> Gr
  ">=" -> GrEq
  "<" -> Lt
  "<=" -> LtEq
  "==" -> Eq
  "/=" -> NotEq

compiledPrim :: CodeStore
compiledPrim = M.fromList (("if", ifPrim):map mkDyadicPrim operators)
  where mkDyadicPrim op = (op, dyadicPrim (mkOp op))

ifPrim :: [Instr]
ifPrim = [Take 3, Push cont, Enter (Arg 1)]
  where cont = Code [Cond [Enter (Arg 2)] [Enter (Arg 3)]]

dyadicPrim :: Op -> [Instr]
dyadicPrim op = [Take 2, Push cont1, Enter (Arg 2)]
  where cont1 = Code [Push cont2, Enter (Arg 1)]
        cont2 = Code [Op op, Return]

type CompEnv = M.Map Name AddrMode

compileSC :: CompEnv -> Supercomb Name -> (Name, [Instr])
compileSC env (name, args, body) = (name, takeArgs ++ instrs)
  where instrs = compileR body env'
        env' = foldr (uncurry M.insert) env (zip args (map Arg [1..]))
        takeArgs = case length args of
          0 -> [] -- CAF
          n -> [Take n]

compileR :: Expr Name -> CompEnv -> [Instr]
compileR e env
  | isArith e = compileB e env [Return]
  | EAp (EAp (EAp (EVar "if") e0) e1) e2 <- e = -- full condition
    let cont = [Cond [Enter (compileA e1 env)] [Enter (compileA e2 env)]]
    in compileB e0 env cont
  | EAp e1 e2 <- e = Push (compileA e2 env):compileR e1 env
  | EVar _ <- e = [Enter (compileA e env)]
  | otherwise = error "compileR: can't do this yet"

isArith :: Expr Name -> Bool
isArith (ENum _) = True
isArith (EAp (EAp (EVar v) _) _) = elem v operators
isArith _ = False

compileA :: Expr Name -> CompEnv -> AddrMode
compileA (EVar v) env = env M.! v
compileA (ENum n) _ = Const n
compileA e env = Code (compileR e env)

compileB :: Expr Name -> CompEnv -> [Instr] -> [Instr]
compileB e env cont = if isArith e
  then case e of
    ENum n -> PushValue (ValueConst n):cont
    EAp (EAp (EVar v) e1) e2 -> compileB e2 env (compileB e1 env (Op (mkOp v):cont))
  else Push (Code cont):compileR e env
