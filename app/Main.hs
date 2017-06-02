module Main
  ( main
  ) where

import Control.Monad (forever)

import Minilisp (minilisp)
import Minilisp.App (evalAppM)
import Minilisp.Terminal as Terminal (configure)

main :: IO ()
main = do
  Terminal.configure
  forever $ do
    putStr "> "
    input <- getLine
    case evalAppM (minilisp input) of
      (result, log) -> do
        case result of
          Right value -> do
            print value
          Left err -> do
            print err
        putStrLn log
