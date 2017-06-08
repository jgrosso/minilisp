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
       (AST(Atom, Int', List), Atom,
        SugaredAST(SugaredApplication, SugaredAtom, SugaredLambda,
                   SugaredLet))
import Minilisp.Error (Error(Error), Type(InvalidArguments))
import Minilisp.Mangle (mkAtom, mkRestricted)
import Minilisp.State (HasState)

type Arity = Int

data Primitive m =
  Primitive Atom
            Arity
            ([AST] -> m AST)

primitives
  :: (MonadError Error m)
  => [Primitive m]
primitives =
  boolPrimitives <> controlPrimitives <> intPrimitives <> listPrimitives
  where
    boolPrimitives =
      [ Primitive
          "="
          2
          (\args ->
             case args of
               [a, b] ->
                 if a == b
                   then return $ Atom "true"
                   else return $ Atom "false"
               _ ->
                 throwError $
                 Error
                   (InvalidArguments
                      "="
                      "two values"
                      (intercalate ", " $ map show args))
                   Nothing)
      ]
    controlPrimitives =
      [ Primitive
          "if"
          3
          (\args ->
             case args of
               [Atom cond, true, false] ->
                 case cond of
                   "false" -> return false
                   "true" -> return true
                   _ ->
                     throwError $
                     Error
                       (InvalidArguments
                          "if"
                          "a boolean and two values"
                          (intercalate ", " $ map show args))
                       Nothing
               _ ->
                 throwError $
                 Error
                   (InvalidArguments
                      "if"
                      "a boolean and two values"
                      (intercalate ", " $ map show args))
                   Nothing)
      ]
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
      , Primitive
          "-"
          2
          (\args ->
             case args of
               [Int' a, Int' b] -> return $ Int' (a - b)
               _ ->
                 throwError $
                 Error
                   (InvalidArguments
                      "-"
                      "two ints"
                      (intercalate ", " $ map show args))
                   Nothing)
      , Primitive
          "*"
          2
          (\args ->
             case args of
               [Int' a, Int' b] -> return $ Int' (a * b)
               _ ->
                 throwError $
                 Error
                   (InvalidArguments
                      "*"
                      "two ints"
                      (intercalate ", " $ map show args))
                   Nothing)
      , Primitive
          "/"
          2
          (\args ->
             case args of
               [Int' a, Int' b] -> return $ Int' (a `div` b)
               _ ->
                 throwError $
                 Error
                   (InvalidArguments
                      "/"
                      "two ints"
                      (intercalate ", " $ map show args))
                   Nothing)
      ]
    listPrimitives =
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

findPrimitive
  :: (MonadError Error m)
  => Atom -> Maybe (Primitive m)
findPrimitive name =
  find (\(Primitive name' _ _) -> name == mkRestricted name') primitives

curryPrimitive
  :: (HasState s, MonadError Error m, MonadState s m)
  => Primitive m -> m SugaredAST
curryPrimitive (Primitive name arity _) = do
  atoms <- replicateM arity mkAtom
  return $
    SugaredLambda
      atoms
      (SugaredApplication
         (SugaredAtom (mkRestricted name))
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
