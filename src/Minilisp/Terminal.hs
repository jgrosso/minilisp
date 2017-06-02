module Minilisp.Terminal
  ( configure
  ) where

import System.IO
       (BufferMode(NoBuffering), hSetBuffering, stdin, stdout)

configure :: IO ()
configure = mapM_ (`hSetBuffering` NoBuffering) [stdin, stdout]
