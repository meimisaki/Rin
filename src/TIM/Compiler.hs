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
  { instrs = mkEnter (initialEnv M.! "main")
  , framePtr = FrameNull
  , dataFramePtr = FrameNull
  , stack = initialStack
  , valueStack = initialValueStack
  , dump = initialDump
  , heap = heap
  , codeStore = globalFramePtr
  , stats = initialStats }
  where scDefs = preludeDefs ++ prog
        compiledCode = map (compileSC initialEnv) scDefs ++ compiledPrim
        initialEnv = M.fromList (zip names ams)
        names = map fst compiledCode
        ams = flip map (zip [1..] compiledCode) $ \(offset, (name, instrs)) ->
          let am = Label name offset
          in if isCAF instrs then Code [Enter am] else am
        (heap, globalFramePtr) = allocateInitialHeap compiledCode

initialStack :: Stack
initialStack = [([], FrameNull)]

initialValueStack :: ValueStack
initialValueStack = []

initialDump :: Dump
initialDump = []

isCAF :: [Instr] -> Bool
isCAF (UpdateMarkers _:_) = False
isCAF _ = True

allocateInitialHeap :: [(Name, [Instr])] -> (H.Heap Frame, CodeStore)
allocateInitialHeap compiledCode = (heap, globalFramePtr)
  where (heap, globalFramePtr) = allocFrame H.empty frame
        frame = map markCAF (zip [1..] compiledCode)
        markCAF (offset, (_, instrs)) = if isCAF instrs
          then (PushMarker offset:instrs, globalFramePtr)
          else (instrs, globalFramePtr)

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

compiledPrim :: [(Name, [Instr])]
compiledPrim = ("if", ifPrim):map mkDyadicPrim operators
  where mkDyadicPrim op = (op, dyadicPrim (mkOp op))

mkTake :: Int -> Int -> [Instr]
mkTake 0 0 = []
mkTake d 0 = [Take d 0]
mkTake d n = [UpdateMarkers n, Take d n]

ifPrim :: [Instr]
ifPrim = mkTake 3 3 ++ [Push cont, Enter (Arg 1)]
  where cont = Code [Cond [Enter (Arg 2)] [Enter (Arg 3)]]

dyadicPrim :: Op -> [Instr]
dyadicPrim op = mkTake 2 2 ++ [Push cont1, Enter (Arg 2)]
  where cont1 = Code [Push cont2, Enter (Arg 1)]
        cont2 = Code [Op op, Return]

type CompEnv = M.Map Name AddrMode

updateEnv :: CompEnv -> [Name] -> [AddrMode] -> CompEnv
updateEnv env xs ams = extend env (zip xs ams)

compileSC :: CompEnv -> Supercomb Name -> (Name, [Instr])
compileSC env (name, args, body) = (name, mkTake d n ++ instrs)
  where env' = updateEnv env args (map Arg [1..])
        (d, instrs) = compileR body env' n
        n = length args

-- TODO: optimize full condition, eliminate jumps and reuse frame slots
compileR :: Expr Name -> CompEnv -> Int -> (Int, [Instr])
compileR e@(isArith -> True) env d = compileB e env d [Return]
compileR (ELet rec defs body) env d = (d', mvs ++ instrs)
  where (dn, mvs, _) = foldr go (d + n, [], d + n) exps
        go e (dn, mvs, slot) = (dn', Move slot am:mvs, slot - 1)
          where (dn', am) = compileU e slot (if rec then env' else env) dn
        env' = updateEnv env xs (map mkIndMode [d + 1..])
        (d', instrs) = compileR body env' dn
        n = length defs
        xs = map fst defs
        exps = map snd defs
-- TODO: optimize saturated application, never check for partial application
compileR (EAp e a@(isAtomic -> True)) env d = (d', Push am:instrs)
  where (d', instrs) = compileR e env d
        am = compileA a env
compileR (EAp eFun eArg) env d = (d2, Move slot am:Push (mkIndMode slot):instrs)
  where (d1, am) = compileU eArg slot env slot
        (d2, instrs) = compileR eFun env d1
        slot = d + 1
compileR e@(EVar _) env d = (d, mkEnter am)
  where am = compileA e env
compileR (EConstr tag arity) env d = (d, instrs)
  where instrs = mkTake arity arity ++ [ReturnConstr tag]
compileR (ECase e alts) env d = (d', Push (Code [Switch cases]):instrs)
  where xs = map (\alt -> compileE alt env d) alts
        (d', instrs) = compileR e env (maximum (map fst xs))
        cases = M.fromList (map snd xs)

-- TODO: eliminate unused variables, don't move them into current frame
compileE :: Alter Name -> CompEnv -> Int -> (Int, (Int, [Instr]))
compileE (tag, xs, body) env d = (d', (tag, mvs ++ instrs))
  where mvs = map (\i -> Move (d + i) (Data i)) [1..n]
        env' = updateEnv env xs (map Arg [d + 1..])
        (d', instrs) = compileR body env' (d + n)
        n = length xs

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
