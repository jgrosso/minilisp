{-# LANGUAGE FlexibleContexts #-}

module Minilisp.Mangle
  ( initialMangler
  , mkAtom
  , mkRestricted
  ) where

import Control.Monad.State (get, modify, MonadState)

import Data.Char (chr, ord)
import Data.Semigroup ((<>))

import Minilisp.AST (Atom)

initialMangler :: Atom
initialMangler = "a"

mkAtom
  :: MonadState String m
  => m String
mkAtom = do
  modify $ map incrementChar
  mkRestricted . ("_" <>) <$> get
  where
    incrementChar = chr . (+ 1) . ord

mkRestricted :: Atom -> Atom
mkRestricted = ("#" <>)
