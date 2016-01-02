module Core.LambdaLift
( lambdaLift
) where

import Common

import Core.AST
import Core.AnnotAST
import Core.FreeVars
import qualified Core.LambdaLift.Abstract.Naive as N
import Core.LambdaLift.Collect
import Core.LambdaLift.DeBruijn
import Core.LambdaLift.Float
import Core.LambdaLift.MFE
import Core.LambdaLift.Separate
import Core.Rename

fullyLazy :: Program Name -> Program Name
fullyLazy = float . rename unAnnot . identifyMFE . deBruijn . separate

-- TODO: eta-abstraction optimize, merge directly nested lambdas
lambdaLift :: Program Name -> Program Name
lambdaLift = collect . rename_ . N.abstract . freeVars . fullyLazy
