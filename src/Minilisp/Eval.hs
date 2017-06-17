{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Minilisp.Eval
  ( eval
  , runIO
  ) where

import Control.Monad.Except (MonadError, throwError)

import Debug.Trace (traceShowId)

import Minilisp.AST
       (AST(Application, Atom, Lambda, List, QuotedAtom), Atom)
import Minilisp.Error
       (Error(Error), Type(FunctionNotFound, InvalidIOAction))
import Minilisp.Primitives (findPrimitive, Primitive(Primitive))

exhaustM
  :: (Eq a, Monad m)
  => (a -> m a) -> a -> m a
exhaustM f x = do
  result <- f x
  if x == result
    then return result
    else exhaustM f result

recursivelyRewrite :: (AST -> AST) -> AST -> AST
recursivelyRewrite f expression =
  f $
  case expression of
    Application fn args ->
      Application (recursivelyRewrite' fn) (map recursivelyRewrite' args)
    Lambda param body -> Lambda param (recursivelyRewrite' body)
    List expressions -> List (map recursivelyRewrite' expressions)
    _ -> expression
  where
    recursivelyRewrite' = recursivelyRewrite f

recursivelyEval
  :: Monad m
  => (AST -> m AST) -> AST -> m AST
recursivelyEval f expression =
  f =<<
  case expression of
    Application fn args ->
      Application <$> recursivelyEval' fn <*> traverse recursivelyEval' args
    List expressions -> List <$> traverse recursivelyEval' expressions
    _ -> return expression
  where
    recursivelyEval' = recursivelyEval f

substitute :: Atom -> AST -> AST -> AST
substitute target value expression@(Atom atom) =
  if atom == target
    then value
    else expression
substitute _ _ expression = expression

evalLambda :: AST -> AST
evalLambda (Application (Lambda param body) (arg:restArgs)) =
  let result = traceShowId $ recursivelyRewrite (substitute param arg) body
  in case restArgs of
       [] -> result
       _ -> Application result restArgs
evalLambda expression = expression

evalPrimitive
  :: MonadError Error m
  => AST -> m AST
evalPrimitive expression@(Application (Atom primitive) args) =
  case findPrimitive primitive of
    Just (Primitive _ _ body) -> body args
    _ ->
      throwError $ Error (FunctionNotFound primitive) (Just $ show expression)
evalPrimitive expression = return expression

eval
  :: MonadError Error m
  => AST -> m AST
eval = exhaustM $ recursivelyEval evalPrimitive . recursivelyRewrite evalLambda

runIO :: AST -> IO ()
runIO expression@(List (action:args)) =
  case action of
    QuotedAtom "io/exit" -> return ()
    QuotedAtom "io/print" ->
      case args of
        [x, next] -> do
          print x
          runIO next
        _ -> print $ InvalidIOAction expression
    _ -> print $ InvalidIOAction expression
runIO expression = print $ InvalidIOAction expression
