module TIM.Types
( Instr (..)
, AddrMode (..)
, ValueAddrMode (..)
, Op (..)
, TIM (..)
, FramePtr (..)
, Stack
, Closure
, Frame
, allocFrame
, getFrame
, updateFrame
, ValueStack
, Dump
, CodeStore
, Stats (..)
, initialStats
, incTickCount
) where

import Prelude hiding (lookup)

import Common

import Data.Heap
import qualified Data.Map as M

data Instr
  = Take Int
  | Enter AddrMode
  | Push AddrMode
  | PushValue ValueAddrMode
  | Return
  | Op Op
  | Cond [Instr] [Instr]
  deriving Show

data AddrMode
  = Arg Int
  | Label Name
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
  , stack :: Stack
  , valueStack :: ValueStack
  , dump :: Dump
  , heap :: Heap Frame
  , codeStore :: CodeStore
  , stats :: Stats }
  deriving Show

data FramePtr
  = FrameAddr Addr
  | FrameInt Int
  | FrameNull
  deriving Show

type Stack = [Closure]

type Closure = ([Instr], FramePtr)

type Frame = [Closure]

allocFrame :: Heap Frame -> Frame -> (Heap Frame, FramePtr)
allocFrame heap xs = (heap', FrameAddr addr)
  where (heap', addr) = alloc heap xs

getFrame :: Heap Frame -> FramePtr -> Int -> Closure
getFrame heap (FrameAddr addr) n = xs !! (n - 1)
  where xs = lookup heap addr

updateFrame :: Heap Frame -> FramePtr -> Int -> Closure -> Heap Frame
updateFrame heap (FrameAddr addr) n closure = update heap addr xs'
  where xs = lookup heap addr
        xs' = take (n - 1) xs ++ [closure] ++ drop n xs

type ValueStack = [Int]

type Dump = ()

type CodeStore = M.Map Name [Instr]

data Stats = Stats { tickCount :: Int } deriving Show

initialStats :: Stats
initialStats = Stats 0

incTickCount :: Stats -> Stats
incTickCount stats = stats { tickCount = tickCount stats + 1 }
