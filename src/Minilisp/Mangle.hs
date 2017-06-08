module Minilisp.Mangle
  ( mkAtom
  , mkRestricted
  ) where

import Control.Lens ((<<%=))
import Control.Monad.State (MonadState)

import Data.Char (chr, ord)
import Data.Semigroup ((<>))

import Minilisp.AST (Atom)
import Minilisp.State (HasState, parameterName)

mkAtom
  :: (HasState s, MonadState s m)
  => m String
mkAtom = mkRestricted <$> (parameterName <<%= fmap incrementChar)
  where
    incrementChar = chr . (+ 1) . ord

mkRestricted :: Atom -> Atom
mkRestricted = ("#" <>)
