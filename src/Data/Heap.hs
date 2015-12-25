module Data.Heap
( Heap
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

type Heap a = ([Addr], M.Map Addr a)

type Addr = Int

nullAddr :: Addr
nullAddr = 0

empty :: Heap a
empty = ([1..], M.empty)

size :: Heap a -> Int
size = M.size . snd

alloc :: Heap a -> a -> (Heap a, Addr)
alloc (addr:xs, assoc) val = ((xs, M.insert addr val assoc), addr)

free :: Heap a -> Addr -> Heap a
free (xs, assoc) addr = (addr:xs, M.delete addr assoc)

lookup :: Heap a -> Addr -> a
lookup (_, assoc) addr = assoc M.! addr

update :: Heap a -> Addr -> a -> Heap a
update (xs, assoc) addr val = (xs, M.insert addr val assoc)
