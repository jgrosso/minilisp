{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Minilisp.Eval
  ( eval
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState)

import Data.Semigroup ((<>))

import Debug.Trace (traceShowId)

import Minilisp.AST
       (AST(Application, Atom, Char', Lambda, List), Atom)
import Minilisp.Error
       (Error(Error),
        Type(FunctionNotFound, InvalidArguments, InvalidApplication))
import Minilisp.Mangle (mkAtom)
import Minilisp.Primitives (findPrimitive, Primitive(Primitive))
import Minilisp.State (State)

substitute :: Atom -> AST -> AST -> AST
substitute target value expression =
  case expression of
    Application fn body -> Application (substitute' fn) (map substitute' body)
    Atom atom ->
      if atom == target
        then value
        else expression
    Lambda param body -> Lambda param (substitute' body)
    List expressions -> List (map substitute' expressions)
    _ -> expression
  where
    substitute' = substitute target value

inline :: AST -> AST
inline (Application fn args) =
  case fn of
    Lambda param body ->
      let result = inline $ substitute param arg body
      in case restArgs of
           [] -> result
           _ -> inline $ Application result restArgs
    _ -> inline $ Application (inline fn) (map inline args)
  where arg:restArgs = map inline args'
inline (List expressions) = List (map inline expressions)
inline expression = expression

eval' :: MonadError Error m => AST -> m AST
eval' expression@(Application (Atom primitive) args) =
  case findPrimitive primitive of
    Just (Primitive _ _ body) -> eval' $ body args
    _ -> throwError $ Error (FunctionNotFound atom) (Just $ show expressions)
eval' expression = expression

eval'
  :: (MonadError Error m, MonadState State m)
  => AST -> m AST
eval' expression@(Application fn' args') = do
  fn <- eval fn'
  args <- traverse eval args'
  result <-
    case fn of
      Lambda param body ->
        case args of
          arg:restArgs ->
            case restArgs of
              [] -> eval body
              _ -> eval (Application body restArgs)
      Atom atom ->
        case findPrimitive atom of
          Just (Primitive _ _ body) -> body args
          _ ->
            throwError $ Error (FunctionNotFound atom) (Just $ show expression)
  eval result
eval' (List expressions) = List <$> traverse eval expressions
eval' expression = return $ inline expression

eval
  :: (MonadError Error m, MonadState State m)
  => AST -> m AST
eval = eval' . traceShowId . inline
