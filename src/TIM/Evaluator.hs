{-# LANGUAGE RecordWildCards #-}

module TIM.Evaluator
( eval
) where

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
  Take d n:instrs' -> if length stack < n
    then error "Too few args for `Take` instruction"
    else state
      { instrs = instrs'
      , framePtr = framePtr'
      , stack = drop n stack
      , heap = heap' }
    where (heap', framePtr') = allocFrame heap frame
          frame = take n stack ++ replicate (d - n) ([], FrameNull)
  Move i am:instrs' -> state
    { instrs = instrs'
    , heap = heap' }
    where heap' = updateFrame heap framePtr i (mkClosure am)
  [Enter am] -> state
    { instrs = instrs'
    , framePtr = framePtr' }
    where (instrs', framePtr') = mkClosure am
  Push am:instrs' -> state
    { instrs = instrs'
    , stack = mkClosure am:stack }
  PushValue vam:instrs' -> state
    { instrs = instrs'
    , valueStack = n:valueStack }
    where n = case vam of
            FramePtr -> let FrameInt n' = framePtr in n'
            ValueConst n' -> n'
  PushMarker x:instrs' -> state
    { instrs = instrs'
    , stack = []
    , dump = (framePtr, x, stack):dump }
  UpdateMarkers n:instrs' -> if m == n
    then state { instrs = instrs' }
    else state
      { stack = stack ++ stack'
      , dump = dump'
      , heap = heap'' }
    where m = length (take n stack)
          heap'' = updateFrame heap framePtr' x (partial, framePtr'')
          (heap', framePtr'') = allocFrame heap stack
          (framePtr', x, stack'):dump' = dump
          partial = map (Push . Arg) [m, m - 1..1] ++ instrs
  [Return] -> case stack of
    [] -> state
      { stack = stack'
      , dump = dump'
      , heap = heap' }
      where heap' = updateFrame heap framePtr' x (intCode, FrameInt n)
            n:_ = valueStack
            (framePtr', x, stack'):dump' = dump
    (instrs', framePtr'):stack' -> state
      { instrs = instrs'
      , framePtr = framePtr'
      , stack = stack' }
  [ReturnConstr tag] -> case stack of
    [] -> state
      { stack = stack'
      , dump = dump'
      , heap = heap' }
      where heap' = updateFrame heap framePtr' x (instrs, framePtr)
            (framePtr', x, stack'):dump' = dump
    (instrs', framePtr'):stack' -> state
      { instrs = instrs'
      , framePtr = framePtr'
      , dataFramePtr = framePtr
      , stack = stack'
      , valueStack = tag:valueStack }
  Op op:instrs' -> state
    { instrs = instrs'
    , valueStack = dyadicPrim op n1 n2:valueStack' }
    where n1:n2:valueStack' = valueStack
  [Cond instrs1 instrs2] -> state
    { instrs = if n == 0 then instrs1 else instrs2
    , valueStack = valueStack' }
    where n:valueStack' = valueStack
  [Switch cases] -> state
    { instrs = cases M.! tag
    , valueStack = valueStack' }
    where tag:valueStack' = valueStack
  where mkClosure am = case am of
          Arg n -> getFrame heap framePtr n
          Data n -> getFrame heap dataFramePtr n
          Label _ offset -> getFrame heap codeStore offset
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
