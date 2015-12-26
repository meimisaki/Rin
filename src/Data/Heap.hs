module Data.Heap
( Heap (..)
, Addr
, nullAddr
, empty
, size
, alloc
, free
, lookup
, update
) where

import Prelude hiding (lookup)

import qualified Data.Map as M

data Heap a = Heap [Addr] (M.Map Addr a)

instance Show a => Show (Heap a) where
  show (Heap _ assoc) = show assoc

type Addr = Int

nullAddr :: Addr
nullAddr = 0

empty :: Heap a
empty = Heap [1..] M.empty

size :: Heap a -> Int
size (Heap _ assoc) = M.size assoc

alloc :: Heap a -> a -> (Heap a, Addr)
alloc (Heap (addr:xs) assoc) val = (Heap xs (M.insert addr val assoc), addr)

free :: Heap a -> Addr -> Heap a
free (Heap xs assoc) addr = Heap (addr:xs) (M.delete addr assoc)

lookup :: Heap a -> Addr -> a
lookup (Heap _ assoc) addr = assoc M.! addr

update :: Heap a -> Addr -> a -> Heap a
update (Heap xs assoc) addr val = Heap xs (M.insert addr val assoc)
