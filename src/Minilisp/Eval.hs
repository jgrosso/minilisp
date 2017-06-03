{-# LANGUAGE FlexibleContexts #-}

module Minilisp.Eval
  ( eval
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState)

import Data.List (intercalate)
import Data.Semigroup ((<>))

import Minilisp.AST
       (AST(Application, Atom, Char', Lambda, List), Atom)
import Minilisp.Error
       (Error(Error),
        Type(FunctionNotFound, InvalidArguments, InvalidApplication))
import Minilisp.Primitives (findPrimitive, Primitive(Primitive))
import Minilisp.State (State)

substitute :: Atom -> AST -> AST -> AST
substitute param arg body =
  case body of
    Application fn args ->
      Application (substitute param arg fn) (map (substitute param arg) args)
    Atom atom ->
      if atom == param
        then arg
        else body
    Lambda param' body' -> Lambda param' (substitute param arg body')
    List expressions -> List (map (substitute param arg) expressions)
    _ -> body

eval
  :: (MonadError Error m, MonadState State m)
  => AST -> m AST
eval expression@(Application fn' args') = do
  fn <- eval fn'
  args <- traverse eval args'
  result <-
    case fn of
      Lambda param body ->
        case args of
          arg:restArgs ->
            let currentResult = substitute param arg body
            in return $
               case restArgs of
                 [] -> currentResult
                 _ -> Application currentResult restArgs
          [] -> return body
      Atom atom ->
        case findPrimitive atom of
          Just (Primitive _ _ body) -> body args
          _ ->
            throwError $ Error (FunctionNotFound atom) (Just $ show expression)
      _ ->
        throwError $
        Error (InvalidApplication (show fn)) (Just $ show expression)
  eval result
eval (List expressions) = List <$> traverse eval expressions
eval expression = return expression
