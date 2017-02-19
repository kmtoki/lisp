{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict, StrictData #-}

module Lisp.Eval where

import Lisp.AST
import Control.Monad
import qualified Data.Map as M

type Env = M.Map String AST

data LispState 
  = LispState {
    ast :: AST,
    env :: Env,
    ref :: Maybe LispState
  }
  deriving Show


eval :: AST -> IO AST
eval a = do
  ls <- evalState $ initLispState { ast = a }
  pure $ ast ls


initLispState = LispState (Atom (0,0) "#nil") M.empty Nothing

evalError :: LispState -> String -> IO LispState
evalError ls msg = do
  let 
    (p, a) = case ast ls of
      Atom p a -> (p, show a)
      Nat p n  -> (p, show n)
      List p l -> (p, show $ ast ls)
  fail $ show p ++ " " ++ msg ++ " " ++ a
  pure ls

evalState :: LispState -> IO LispState
evalState ls = case ast ls of
  Atom _ a -> pure $ ls { ast = lookup $ env ls }
    where
      lookup e = case M.lookup a e of
        Just a' -> a'
        Nothing -> case ref ls of
          Just e' -> lookup $ env e'
          Nothing -> ast ls

  Nat _ a -> pure ls

  List _ [] -> pure ls

  List _ (Atom _ "do" : body) -> foldM go ls body
    where
      go ls' expr = evalState $ ls' { ast = expr }

  List _ [Atom _ "def", Atom _ name, body] -> 
    pure $ ls { env = M.insert name body $ env ls }

  List pos (Atom _ "def" : _) -> 
    evalError ls "syntax error"

  List _ [Atom _ "lam", List _ names, body] ->
    pure ls

  List pos (Atom _ "lam" : _) ->
    evalError ls "syntax error"

  List _ [Atom pos "+", a, b] -> do
    a' <- evalState $ ls { ast = a }
    b' <- evalState $ ls { ast = b }

    case (ast a', ast b') of
      (Nat _ a, Nat _ b) -> pure $ ls { ast = Nat pos (a + b) }
      _ -> evalError ls "+ expect nat"

  List _ [Atom pos "-", a, b] -> do
    a' <- evalState $ ls { ast = a }
    b' <- evalState $ ls { ast = b }

    case (ast a', ast b') of
      (Nat _ a, Nat _ b) -> pure $ ls { ast = Nat pos (a - b) }
      _ -> evalError ls "- expect nat"

  List _ (Atom _ "puts" : args) -> mapM_ puts args >> pure ls
    where
      puts arg = do
        a <- evalState $ ls { ast = arg }
        print a

  List _ [Atom _ "if", bool, true, false] -> do
    b <- evalState $ ls { ast = bool }
    case ast b of
      Atom _ "#t" -> evalState $ ls { ast = true }
      Atom _ "#f" -> evalState $ ls { ast = false }
      _ -> evalError ls "if expect #t or #f"

  List pos [Atom _ "atom", a] -> do 
    a' <- evalState $ ls { ast = a }
    case ast a' of
      Atom _ _ -> pure $ ls { ast = Atom pos "#t" }
      List _ _ -> pure $ ls { ast = Atom pos "#f" }

  List _ ((Atom _ "atom"):_) ->
    evalError ls "syntax error"

  List _ [Atom pos "eq", a, b] -> do
    a' <- evalState $ ls { ast = a }
    b' <- evalState $ ls { ast = b }
    if ast a' == ast b' then
      pure $ ls { ast = Atom pos "#t" }
    else
      pure $ ls { ast = Atom pos "#f" }

  List _ ((Atom _ "eq"):_) ->
    evalError ls "syntax error"

  List p1 [Atom p2 "cons", a, b] -> do
    a' <- evalState $ ls { ast = a }
    b' <- evalState $ ls { ast = b }
    pure $ ls { ast = List p1 [Atom p2 "cons", ast a', ast b'] }

  List p1 ((Atom p2 "cons"):_) ->
    evalError ls "syntax error"

  List _ [Atom _ "car", a] -> do
    a' <- evalState $ ls { ast = a }
    case ast a' of
      List _ [Atom _ "cons", a'', b] -> pure $ ls { ast = a'' }
      _ -> evalError ls "car expect cons"

  List _ ((Atom _ "car"):_) ->
    evalError ls "syntax error"

  List _ [Atom _ "cdr", a] -> do
    a' <- evalState $ ls { ast = a }
    case ast a' of
      List _ [Atom _ "cons", a'', b] -> pure $ ls { ast = b }
      _ -> evalError ls "car expect cons"

  List _ ((Atom _ "cdr"):_) ->
    evalError ls "syntax error"

  -- apply
  List pos (lambda : args) -> do
    lambda' <- evalState $ ls { ast = lambda }
    case ast lambda' of
      List _ [Atom _ "lam", List _ names, body] ->
        if length names == length args then do
          args' <- mapM (\a -> ast <$> (evalState $ ls { ast = a })) args
          let env' = M.union (M.fromList $ zip (map (\(Atom _ n) -> n) names) args') $ env ls
          evalState $ ls { ast = body, env = env' }
        else
          evalError ls $ 
            "lam expect " ++ (show $ length names) ++ " unexpect " ++ (show $ length args)

      _ -> evalError lambda' "apply expect lam"
