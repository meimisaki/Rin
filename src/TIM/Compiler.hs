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
  , codeStore = M.union compiledCode compiledPrimitives
  , stats = initialStats }
  where scDefs = preludeDefs ++ prog
        compiledCode = M.fromList (map (compileSC initialEnv) scDefs)
        initialEnv = M.fromList [(name, Label name) | name <- names]
        names = [name | (name, _, _) <- scDefs] ++ M.keys compiledPrimitives

initialStack :: Stack
initialStack = []

initialValueStack :: ValueStack
initialValueStack = ()

initialDump :: Dump
initialDump = ()

compiledPrimitives :: CodeStore
compiledPrimitives = M.fromList
  []

type CompEnv = M.Map Name AddrMode

compileSC :: CompEnv -> Supercomb Name -> (Name, [Instr])
compileSC = undefined
