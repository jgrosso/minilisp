module Main
  ( main
  ) where

import Control.Monad (forever)

import Minilisp.Logging (Verbosity(Quiet))
import Minilisp.Repl
       (Configuration(Configuration, _verbosity), repl)
import Minilisp.Terminal as Terminal (configure)

main :: IO ()
main = do
  Terminal.configure
  repl Configuration {_verbosity = Quiet}
