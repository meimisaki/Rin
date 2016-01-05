module Core.LambdaLift
( fullyLazy
, lambdaLift
, lambdaLiftJ
, lambdaLiftJ_
) where

import Common

import Core.AST
import Core.AnnotAST
import Core.FreeVars
import qualified Core.LambdaLift.Abstract.Naive as N
import qualified Core.LambdaLift.Abstract.Johnsson as J
import Core.LambdaLift.Collect
import Core.LambdaLift.DeBruijn
import Core.LambdaLift.Float
import Core.LambdaLift.MFE
import Core.LambdaLift.Separate
import Core.Rename
import qualified Data.Set as S

fullyLazy :: Program Name -> Program Name
fullyLazy = float . rename unAnnot . identifyMFE . deBruijn . separate

lambdaLift :: Program Name -> Program Name
lambdaLift = collect . rename_ . N.abstract . freeVars

lambdaLiftJ :: Program Name -> Program Name
lambdaLiftJ = lambdaLiftJ_ . rename_

lambdaLiftJ_ :: Program Name -> Program Name
lambdaLiftJ_ = collect . rename_ . J.abstract . freeVars
