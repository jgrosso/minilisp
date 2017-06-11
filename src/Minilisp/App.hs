{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minilisp.App
  ( AppM
  , evalAppM
  ) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.State (evalStateT, MonadState, StateT)
import Control.Monad.Writer (MonadWriter, runWriter, Writer)

import Minilisp.Error (Error)
import Minilisp.Logging (Log)
import Minilisp.Mangle (initialMangler)
import Minilisp.State (_parameterName, State(State))

newtype AppM a = AppM
  { runAppM :: StateT State (ExceptT Error (Writer Log)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError Error
             , MonadState State
             , MonadWriter Log
             )

evalAppM :: AppM a -> (Either Error a, Log)
evalAppM = runWriter . runExceptT . flip evalStateT initialState . runAppM
  where
    initialState = State {_parameterName = initialMangler}
