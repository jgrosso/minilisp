module Minilisp.Mangle
  ( mkAtom
  ) where

import Control.Lens ((<<%=))
import Control.Monad.State (MonadState)

import Data.Char (chr, ord)

import Minilisp.State (HasState, parameterName)

mkAtom
  :: (HasState s, MonadState s m)
  => m String
mkAtom = parameterName <<%= fmap incrementChar
  where
    incrementChar = chr . (+ 1) . ord
