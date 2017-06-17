{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minilisp.App
  ( AppM
  , evalAppM
  ) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Writer (MonadWriter, runWriter, Writer)

import Minilisp.Error (Error)
import Minilisp.Logging (Log)

newtype AppM a = AppM
  { runAppM :: ExceptT Error (Writer Log) a
  } deriving (Functor, Applicative, Monad, MonadError Error, MonadWriter Log)

evalAppM :: AppM a -> (Either Error a, Log)
evalAppM = runWriter . runExceptT . runAppM
