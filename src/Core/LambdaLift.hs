module Core.LambdaLift
( lambdaLift
, lambdaLiftJ
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

mkLifter :: (AnnotProgram (S.Set Name) Name -> Program Name) -> Program Name -> Program Name
mkLifter abstract = collect . rename_ . abstract . freeVars . fullyLazy

lambdaLift :: Program Name -> Program Name
lambdaLift = mkLifter N.abstract

lambdaLiftJ :: Program Name -> Program Name
lambdaLiftJ = mkLifter J.abstract
