module Core.LambdaLift
( lambdaLift
) where

import Common

import Core.AST
import Core.FreeVars
import qualified Core.LambdaLift.Abstract.Naive as N
import Core.LambdaLift.Collect
import Core.Rename

-- TODO: eta-abstraction optimize, merge directly nested lambdas
lambdaLift :: Program Name -> Program Name
lambdaLift = collect . rename_ . N.abstract . freeVars
