{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Minilisp.Stdlib
  ( wrapWithStdlib
  ) where

import Control.Monad.Except (MonadError)

import Minilisp.AST (Atom, SugaredAST(SugaredLet))
import Minilisp.Error (Error)
import Minilisp.Parse (parse)
import Minilisp.Stages (normalizeAST)

stdlib
  :: MonadError Error m
  => m [(Atom, SugaredAST)]
stdlib = mapM (\(name, body) -> (name, ) <$> (parse body >>= normalizeAST)) defs
  where
    defs =
      [ ("!", "(lambda (x) (if x false true))")
      , ("&&", "(lambda (a b) (if a (if b true false) false))")
      , ("||", "(lambda (a b) (if a true (if b true false)))")
      ]

wrapWithStdlib
  :: MonadError Error m
  => SugaredAST -> m SugaredAST
wrapWithStdlib ast = SugaredLet <$> stdlib <*> pure ast
