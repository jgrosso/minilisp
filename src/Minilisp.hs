{-# LANGUAGE FlexibleContexts #-}

module Minilisp
  ( minilisp
  ) where

import Data.Semigroup ((<>))

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, tell, Writer)

import Minilisp.AST (AST)
import Minilisp.App (AppM)
import Minilisp.Error (Error)
import Minilisp.Eval (eval)
import Minilisp.Parse (parse)
import Minilisp.Primitives (wrapWithCurriedPrimitives)
import Minilisp.Stages (desugarAST, normalizeAST)
import Minilisp.State (State)

minilisp
  :: (MonadError Error m, MonadState State m, MonadWriter String m)
  => String -> m AST
minilisp input = do
  rawAST <- parse input
  tell $ "Raw AST: " <> show rawAST <> "\n"
  sugaredAST <- normalizeAST rawAST
  tell $ "Sugared AST: " <> show sugaredAST <> "\n"
  wrappedAST <- wrapWithCurriedPrimitives sugaredAST
  tell $ "Wrapped AST: " <> show wrappedAST <> "\n"
  ast <- desugarAST wrappedAST
  tell $ "AST: " <> show ast <> "\n"
  eval ast
