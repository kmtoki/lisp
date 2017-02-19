{-# LANGUAGE Strict, StrictData #-}
module Lisp.AST where

import Data.List

type Pos = (Int,Int)
data AST
  = Atom Pos String
  | Nat Pos Int
  | List Pos [AST]

instance Show AST where
  show (Atom _ s) = s
  show (Nat _ n) = show n
  show (List _ ls) = "(" ++ (concat $ intersperse " " $ map show ls) ++ ")"

instance Eq AST where
  Atom _ a == Atom _ b = a == b
  Nat _ a  == Nat _ b = a == b
  List _ a == List _ b = a == b
  a == b = False
