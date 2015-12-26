{-# LANGUAGE ViewPatterns #-}

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
ifPrim = [Take 3 3, Push cont, Enter (Arg 1)]
  where cont = Code [Cond [Enter (Arg 2)] [Enter (Arg 3)]]

dyadicPrim :: Op -> [Instr]
dyadicPrim op = [Take 2 2, Push cont1, Enter (Arg 2)]
  where cont1 = Code [Push cont2, Enter (Arg 1)]
        cont2 = Code [Op op, Return]

type CompEnv = M.Map Name AddrMode

-- TODO: optimize CAF
compileSC :: CompEnv -> Supercomb Name -> (Name, [Instr])
compileSC env (name, args, body) = (name, Take d n:instrs)
  where (d, instrs) = compileR body env' n
        env' = foldr (uncurry M.insert) env (zip args (map Arg [1..]))
        n = length args

compileR :: Expr Name -> CompEnv -> Int -> (Int, [Instr])
compileR e@(isArith -> True) env d = compileB e env d [Return]
compileR (ELet rec defs body) env d = (d', mvs ++ instrs)
  where (dn, mvs, _) = foldr go (d + n, [], d + n) exps
        go e (dn, mvs, slot) = (dn', Move slot am:mvs, slot - 1)
          where (dn', am) = compileA e (if rec then env' else env) dn
        env' = foldr (uncurry M.insert) env (zip xs ams)
        ams = map (if rec then mkIndMode else Arg) [d + 1..]
        (d', instrs) = compileR body env' dn
        n = length defs
        xs = map fst defs
        exps = map snd defs
compileR (ECond e0 e1 e2) env d = (max d0 (max d1 d2), instrs)
  where (d0, instrs) = compileB e0 env d [Cond [Enter am1] [Enter am2]]
        (d1, am1) = compileA e1 env d
        (d2, am2) = compileA e2 env d
compileR (EAp e1 e2) env d = (d2, Push am:instrs)
  where (d1, am) = compileA e2 env d
        (d2, instrs) = compileR e1 env d1
compileR e@(EVar _) env d = (d', [Enter am])
  where (d', am) = compileA e env d
compileR _ _ _ = error "compileR: can't do this yet"

mkIndMode :: Int -> AddrMode
mkIndMode n = Code [Enter (Arg n)]

isArith :: Expr Name -> Bool
isArith (ENum _) = True
isArith (EAp (EAp (EVar v) _) _) = elem v operators
isArith _ = False

compileA :: Expr Name -> CompEnv -> Int -> (Int, AddrMode)
compileA (EVar v) env d = (d, env M.! v)
compileA (ENum n) _ d = (d, Const n)
compileA e env d = (d', Code instrs)
  where (d', instrs) = compileR e env d

compileB :: Expr Name -> CompEnv -> Int -> [Instr] -> (Int, [Instr])
compileB e env d cont = if isArith e
  then case e of
    ENum n -> (d, PushValue (ValueConst n):cont)
    EAp (EAp (EVar v) e1) e2 -> (max d1 d2, instrs2)
      where (d1, instrs1) = compileB e1 env d (Op (mkOp v):cont)
            (d2, instrs2) = compileB e2 env d instrs1
  else (d', Push (Code cont):instrs)
    where (d', instrs) = compileR e env d
