module TIM.Types where

import Common

import qualified Data.Heap as H
import qualified Data.Map as M

data Instr
  = Take Int Int -- allocation, arity
  | Move Int AddrMode
  | Enter AddrMode
  | Push AddrMode
  | PushValue ValueAddrMode
  | PushMarker Int
  | UpdateMarkers Int
  | Return
  | ReturnConstr Int
  | Op Op
  | Cond [Instr] [Instr]
  | Switch (M.Map Int [Instr])
  deriving Show
-- TODO: remove `Cond` instruction in favor of `Switch`

data AddrMode
  = Arg Int
  | Data Int
  | Label Name Int
  | Code [Instr]
  | Const Int
  deriving Show

data ValueAddrMode
  = FramePtr
  | ValueConst Int
  deriving Show

data Op
  = Add | Sub | Mult | Div | Neg
  | Gr | GrEq | Lt | LtEq | Eq | NotEq
  deriving Show

data TIM = TIM
  { instrs :: [Instr]
  , framePtr :: FramePtr
  , dataFramePtr :: FramePtr
  , stack :: Stack
  , valueStack :: ValueStack
  , dump :: Dump
  , heap :: H.Heap Frame
  , codeStore :: CodeStore
  , stats :: Stats }
  deriving Show

data FramePtr
  = FrameAddr H.Addr
  | FrameInt Int
  | FrameNull
  deriving Show

type Stack = [Closure]

type Closure = ([Instr], FramePtr)

type Frame = [Closure]

allocFrame :: H.Heap Frame -> Frame -> (H.Heap Frame, FramePtr)
allocFrame heap xs = (heap', FrameAddr addr)
  where (heap', addr) = H.alloc heap xs

getFrame :: H.Heap Frame -> FramePtr -> Int -> Closure
getFrame heap (FrameAddr addr) n = xs !! (n - 1)
  where xs = H.lookup heap addr

updateFrame :: H.Heap Frame -> FramePtr -> Int -> Closure -> H.Heap Frame
updateFrame heap (FrameAddr addr) n closure = H.update heap addr xs'
  where xs = H.lookup heap addr
        xs' = take (n - 1) xs ++ [closure] ++ drop n xs

type ValueStack = [Int]

type Dump = [(FramePtr, Int, Stack)]

type CodeStore = FramePtr

data Stats = Stats { tickCount :: Int }
  deriving Show

initialStats :: Stats
initialStats = Stats 0

incTickCount :: Stats -> Stats
incTickCount stats = stats { tickCount = tickCount stats + 1 }
