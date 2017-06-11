module Minilisp.Mangle
  ( initialMangler
  , mkAtom
  , mkRestricted
  ) where

import Control.Lens ((<<%=))
import Control.Monad.State (MonadState)

import Data.Char (chr, ord)
import Data.Semigroup ((<>))

import Minilisp.AST (Atom)
import Minilisp.State (HasState, parameterName)

initialMangler :: Atom
initialMangler = "a"

mkAtom
  :: (HasState s, MonadState s m)
  => m String
mkAtom = (mkRestricted . ("_" <>)) <$> (parameterName <<%= fmap incrementChar)
  where
    incrementChar = chr . (+ 1) . ord

mkRestricted :: Atom -> Atom
mkRestricted = ("#" <>)
