{-# LANGUAGE InstanceSigs #-}

module Minilisp.Error
  ( Context
  , Error(Error)
  , Type(EmptyList, FunctionNotFound, InvalidArguments,
     InvalidApplication, InvalidSyntax)
  ) where

import Data.List (intercalate)
import Data.Semigroup ((<>))

import Minilisp.AST (AST, Atom)

type Context = String

data Type
  = EmptyList
  | FunctionNotFound Atom
  | InvalidArguments Atom
                     String
                     String
  | InvalidApplication String
  | InvalidLambda
  | InvalidSyntax String

data Error =
  Error Type
        (Maybe Context)

instance Show Error where
  show :: Error -> String
  show (Error type' context') =
    show type' <>
    case context' of
      Just context -> "\nCurrent Frame: " <> context
      Nothing -> ""

instance Show Type where
  show :: Type -> String
  show EmptyList = mkError "Lists must not be empty"
  show (FunctionNotFound fn) = mkError $ "Unrecognized function: " <> fn
  show (InvalidArguments fn expectedArgs actualArgs) =
    mkError $
    fn <> " takes " <> expectedArgs <> " as arguments, not: " <> actualArgs
  show (InvalidApplication nonFn) =
    mkError $
    show nonFn <> " is not a function and so cannot be applied to arguments"
  show (InvalidSyntax msg) = mkError msg

mkError :: String -> String
mkError = (<> "!")
