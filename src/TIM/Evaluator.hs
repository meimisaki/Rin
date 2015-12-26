{-# LANGUAGE RecordWildCards #-}

module TIM.Evaluator
( eval
) where

import qualified Data.Heap as H
import qualified Data.Map as M
import TIM.Types

final :: TIM -> Bool
final (TIM { instrs = [] }) = True
final _ = False

eval :: TIM -> [TIM]
eval state = state:rest
  where rest = if final state then [] else eval next
        next = doAdmin (step state)

-- TODO: garbage collection
doAdmin :: TIM -> TIM
doAdmin state = state { stats = incTickCount (stats state) }

step :: TIM -> TIM
step state@(TIM {..}) = case instrs of
  Take n:instrs' -> if length stack < n
    then error "Too few args for `Take` instruction"
    else state
      { instrs = instrs'
      , framePtr = framePtr'
      , stack = drop n stack
      , heap = heap' }
    where (heap', framePtr') = allocFrame heap (take n stack)
  [Enter am] -> state
    { instrs = instrs'
    , framePtr = framePtr' }
    where (instrs', framePtr') = mkClosure am framePtr heap codeStore
  Push am:instrs' -> state
    { instrs = instrs'
    , stack = closure:stack }
    where closure = mkClosure am framePtr heap codeStore
  PushValue vam:instrs' -> state
    { instrs = instrs'
    , valueStack = n:valueStack }
    where n = case vam of
            FramePtr -> let FrameInt n' = framePtr in n'
            ValueConst n' -> n'
  [Return] -> state
    { instrs = instrs'
    , framePtr = framePtr'
    , stack = stack' }
    where (instrs', framePtr'):stack' = stack
  Op op:instrs' -> state
    { instrs = instrs'
    , valueStack = dyadicPrim op n1 n2:valueStack' }
    where n1:n2:valueStack' = valueStack
  [Cond instrs1 instrs2] -> state
    { instrs = if n == 0 then instrs1 else instrs2
    , valueStack = valueStack' }
    where n:valueStack' = valueStack

mkClosure :: AddrMode -> FramePtr -> H.Heap Frame -> CodeStore -> Closure
mkClosure am framePtr heap codeStore = case am of
  Arg n -> getFrame heap framePtr n
  Label name -> (codeStore M.! name, framePtr )
  Code instrs -> (instrs, framePtr)
  Const n -> (intCode, FrameInt n)

intCode :: [Instr]
intCode = [PushValue FramePtr, Return]

dyadicPrim :: Op -> Int -> Int -> Int
dyadicPrim op n1 n2 = case op of
  Add -> n1 + n2
  Sub -> n1 - n2
  Mult -> n1 * n2
  Div -> n1 `div` n2
  Gr -> if n1 > n2 then t else f
  GrEq -> if n1 >= n2 then t else f
  Lt -> if n1 < n2 then t else f
  LtEq -> if n1 <= n2 then t else f
  Eq -> if n1 == n2 then t else f
  NotEq -> if n1 /= n2 then t else f
  where t = 0
        f = 1
