module Core.Prelude
( operators
, preludeDefs
) where

import Common

import Core.AST

operators :: [Name]
operators = ["+", "-", "*", "/", ">", ">=", "<", "<=", "==", "/="]

preludeDefs :: Program Name
preludeDefs = Program
  [ Supercomb "I" ["x"] (EVar "x")
  , Supercomb "K" ["x", "y"] (EVar "x")
  , Supercomb "K1" ["x", "y"] (EVar "y")
  , Supercomb "S" ["f", "g", "x"] (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
  , Supercomb "compose" ["f", "g", "x"] (EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
  , Supercomb "twice" ["f"] (EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]
