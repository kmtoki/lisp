module Main where

import Lisp
import System.Environment

main = do
  args <- getArgs
  case args of
    ["-e", sexpr] -> runLispFile sexpr
    [file] -> runLispFile file
