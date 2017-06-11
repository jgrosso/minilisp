{-# LANGUAGE FlexibleContexts #-}

module Minilisp
  ( minilisp
  ) where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter)

import Data.Semigroup ((<>))

import Minilisp.AST (AST)
import Minilisp.App (AppM)
import Minilisp.Error (Error)
import Minilisp.Eval (eval)
import Minilisp.Logging (Log, logMessage, LogMessage(PassMessage))
import Minilisp.Parse (parse)
import Minilisp.Primitives (wrapWithCurriedPrimitives)
import Minilisp.Stages (desugarAST, normalizeAST)
import Minilisp.State (State)
import Minilisp.Stdlib (wrapWithStdlib)

minilisp
  :: (MonadError Error m, MonadState State m, MonadWriter Log m)
  => String -> m AST
minilisp input = do
  rawAST <- parse input
  logMessage $ PassMessage $ "Raw AST: " <> show rawAST <> "\n"
  sugaredAST <- normalizeAST rawAST
  logMessage $ PassMessage $ "Sugared AST: " <> show sugaredAST <> "\n"
  stdlibWrappedAST <- wrapWithStdlib sugaredAST
  logMessage $
    PassMessage $ "Stdlib-Wrapped AST: " <> show stdlibWrappedAST <> "\n"
  primitiveWrappedAST <- wrapWithCurriedPrimitives stdlibWrappedAST
  logMessage $
    PassMessage $ "Primitive-Wrapped AST: " <> show primitiveWrappedAST <> "\n"
  ast <- desugarAST primitiveWrappedAST
  logMessage $ PassMessage $ "AST: " <> show ast <> "\n"
  eval ast
