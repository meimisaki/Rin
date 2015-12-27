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
initialDump = []

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

mkTake :: Int -> Int -> [Instr]
mkTake 0 0 = []
mkTake d n = [UpdateMarkers n, Take d n]

ifPrim :: [Instr]
ifPrim = mkTake 3 3 ++ [Push cont, Enter (Arg 1)]
  where cont = Code [Cond [Enter (Arg 2)] [Enter (Arg 3)]]

dyadicPrim :: Op -> [Instr]
dyadicPrim op = mkTake 2 2 ++ [Push cont1, Enter (Arg 2)]
  where cont1 = Code [Push cont2, Enter (Arg 1)]
        cont2 = Code [Op op, Return]

type CompEnv = M.Map Name AddrMode

compileSC :: CompEnv -> Supercomb Name -> (Name, [Instr])
compileSC env (name, args, body) = (name, mkTake d n ++ instrs)
  where (d, instrs) = compileR body env' n
        env' = foldr (uncurry M.insert) env (zip args (map Arg [1..]))
        n = length args

-- TODO: optimize full condition
compileR :: Expr Name -> CompEnv -> Int -> (Int, [Instr])
compileR e@(isArith -> True) env d = compileB e env d [Return]
compileR (ELet rec defs body) env d = (d', mvs ++ instrs)
  where (dn, mvs, _) = foldr go (d + n, [], d + n) exps
        go e (dn, mvs, slot) = (dn', Move slot am:mvs, slot - 1)
          where (dn', am) = compileU e slot (if rec then env' else env) dn
        env' = foldr (uncurry M.insert) env (zip xs (map mkIndMode [d + 1..]))
        (d', instrs) = compileR body env' dn
        n = length defs
        xs = map fst defs
        exps = map snd defs
compileR (EAp e a@(isAtomic -> True)) env d = (d', Push am:instrs)
  where (d', instrs) = compileR e env d
        am = compileA a env
compileR (EAp eFun eArg) env d = (d2, Move slot am:Push (mkIndMode slot):instrs)
  where (d1, am) = compileU eArg slot env slot
        (d2, instrs) = compileR eFun env d1
        slot = d + 1
compileR e@(EVar _) env d = (d, mkEnter am)
  where am = compileA e env
compileR _ _ _ = error "compileR: can't do this yet"

isAtomic :: Expr Name -> Bool
isAtomic (EVar _) = True
isAtomic (ENum _) = True
isAtomic _ = False

compileU :: Expr Name -> Int -> CompEnv -> Int -> (Int, AddrMode)
compileU (ENum n) _ _ d = (d, Const n)
compileU e u env d = (d', Code (PushMarker u:instrs))
  where (d', instrs) = compileR e env d

mkEnter :: AddrMode -> [Instr]
mkEnter (Code instrs) = instrs
mkEnter am = [Enter am]

mkIndMode :: Int -> AddrMode
mkIndMode n = Code [Enter (Arg n)]

isArith :: Expr Name -> Bool
isArith (ENum _) = True
isArith (EAp (EAp (EVar v) _) _) = elem v operators
isArith _ = False

compileA :: Expr Name -> CompEnv -> AddrMode
compileA (EVar v) env = env M.! v
compileA (ENum n) _ = Const n

compileB :: Expr Name -> CompEnv -> Int -> [Instr] -> (Int, [Instr])
compileB e env d cont = if isArith e
  then case e of
    ENum n -> (d, PushValue (ValueConst n):cont)
    EAp (EAp (EVar v) e1) e2 -> (max d1 d2, instrs2)
      where (d1, instrs1) = compileB e1 env d (Op (mkOp v):cont)
            (d2, instrs2) = compileB e2 env d instrs1
  else (d', Push (Code cont):instrs)
    where (d', instrs) = compileR e env d
