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

doAdmin :: TIM -> TIM
doAdmin state = state { stats = incTickCount (stats state) }

step :: TIM -> TIM
step state@(TIM {..}) = case instrs of
  Take n:instrs' -> if length stack < n
    then error "Too few args for Take instruction"
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

mkClosure :: AddrMode -> FramePtr -> H.Heap Frame -> CodeStore -> Closure
mkClosure am framePtr heap codeStore = case am of
  Arg n -> getFrame heap framePtr n
  Label name -> (codeStore M.! name, framePtr )
  Code instrs -> (instrs, framePtr)
  Const n -> (intCode, FrameInt n)

intCode :: [Instr]
intCode = []
