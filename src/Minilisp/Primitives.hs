{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Minilisp.Primitives
  ( Arity
  , findPrimitive
  , Primitive(Primitive)
  , wrapWithCurriedPrimitives
  ) where

import Control.Monad (replicateM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState)
import Data.List (find, intercalate)
import Data.Semigroup ((<>))

import Minilisp.AST
       (AST(Int', List), Atom,
        SugaredAST(SugaredApplication, SugaredAtom, SugaredLambda,
                   SugaredLet))
import Minilisp.Error (Error(Error), Type(InvalidArguments))
import Minilisp.Mangle (mkAtom)
import Minilisp.State (HasState)

type Arity = Int

data Primitive m =
  Primitive Atom
            Arity
            ([AST] -> m AST)

primitives
  :: (MonadError Error m)
  => [Primitive m]
primitives = intPrimitives ++ stringPrimitives
  where
    intPrimitives =
      [ Primitive
          "+"
          2
          (\args ->
             case args of
               [Int' a, Int' b] -> return $ Int' (a + b)
               _ ->
                 throwError $
                 Error
                   (InvalidArguments
                      "+"
                      "two ints"
                      (intercalate ", " $ map show args))
                   Nothing)
      ]
    stringPrimitives =
      [ Primitive
          "<>"
          2
          (\args ->
             case args of
               [List a, List b] -> return $ List (a <> b)
               _ ->
                 throwError $
                 Error
                   (InvalidArguments
                      "<>"
                      "two lists"
                      (intercalate ", " $ map show args))
                   Nothing)
      ]

mkPrimitiveName :: String -> String
mkPrimitiveName = ("#" <>)

findPrimitive
  :: (MonadError Error m)
  => Atom -> Maybe (Primitive m)
findPrimitive name =
  find (\(Primitive name' _ _) -> name == mkPrimitiveName name') primitives

curryPrimitive
  :: (HasState s, MonadError Error m, MonadState s m)
  => Primitive m -> m SugaredAST
curryPrimitive (Primitive name arity _) = do
  atoms <- replicateM arity mkAtom
  return $
    SugaredLambda
      atoms
      (SugaredApplication
         (SugaredAtom (mkPrimitiveName name))
         (map SugaredAtom atoms))

wrapWithCurriedPrimitives
  :: (HasState s, MonadError Error m, MonadState s m)
  => SugaredAST -> m SugaredAST
wrapWithCurriedPrimitives ast =
  SugaredLet <$>
  traverse
    (\primitive@(Primitive name _ _) -> (name, ) <$> curryPrimitive primitive)
    primitives <*>
  pure ast
