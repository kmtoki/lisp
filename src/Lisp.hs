{-# LANGUAGE Strict, StrictData #-}
module Lisp where

import Lisp.AST
import Lisp.Parser
import Lisp.Eval

import System.Directory

runLisp' :: String -> String -> IO ()
runLisp' fn s = do
  case parse s of
    Left err -> print err
    Right ast -> do
      ast' <- eval ast
      print ast'

runLisp = runLisp' "runLisp"

runLispFile f = do
  bool <- doesFileExist f
  if bool then do
    s <- readFile f
    runLisp' f s
  else
    fail $ f ++ " does not exist"
