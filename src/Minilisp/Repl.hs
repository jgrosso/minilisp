{-# LANGUAGE TemplateHaskell #-}

module Minilisp.Repl
  ( Configuration(Configuration, _verbosity)
  , repl
  ) where

import Control.Lens ((^.), makeClassy)
import Control.Monad (forever)

import Minilisp (minilisp)
import Minilisp.App (evalAppM)
import Minilisp.Eval (runIO)
import Minilisp.Logging (showLog, Verbosity)

newtype Configuration = Configuration
  { _verbosity :: Verbosity
  }

makeClassy ''Configuration

repl
  :: HasConfiguration a
  => a -> IO ()
repl config =
  forever $ do
    putStr "> "
    input <- getLine
    case evalAppM (minilisp input) of
      (result, log') -> do
        either print runIO result
        putStrLn $ showLog (config ^. verbosity) log'
