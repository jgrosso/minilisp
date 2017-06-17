{-# LANGUAGE FlexibleContexts #-}

module Minilisp.Logging
  ( Log
  , logMessage
  , LogMessage(EvalEnvMessage, PassMessage)
  , showLog
  , Verbosity(Quiet, Verbose)
  ) where

import Control.Monad.Writer (MonadWriter, tell)

import Data.List (intercalate)
import Data.Maybe (mapMaybe)

import Minilisp.AST (AST, Atom)

data Verbosity
  = Quiet
  | Verbose
  deriving (Eq)

data LogMessage
  = EvalEnvMessage [(Atom, AST)]
  | PassMessage String

type Log = [LogMessage]

logMessage
  :: MonadWriter Log m
  => LogMessage -> m ()
logMessage = tell . pure

showLog :: Verbosity -> Log -> String
showLog verbosity = intercalate "\n" . mapMaybe shouldShowMessage
  where
    shouldShowMessage (EvalEnvMessage env) =
      case verbosity of
        Quiet -> Nothing
        Verbose -> Just $ show env
    shouldShowMessage (PassMessage message) =
      case verbosity of
        Quiet -> Nothing
        Verbose -> Just message
