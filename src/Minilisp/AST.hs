{-# LANGUAGE InstanceSigs #-}

module Minilisp.AST
  ( AST(Application, Atom, Char', Int', Lambda, String')
  , Atom
  , RawAST(RawAtom, RawChar, RawInt, RawList, RawString)
  , SugaredAST(SugaredApplication, SugaredAtom, SugaredChar,
           SugaredInt, SugaredLambda, SugaredLet, SugaredString)
  ) where

import Data.List (intercalate)
import Data.Semigroup ((<>))

type Atom = String

data RawAST
  = RawAtom Atom
  | RawChar Char
  | RawInt Int
  | RawList [RawAST]
  | RawString String

instance Show RawAST where
  show :: RawAST -> String
  show (RawAtom atom) = atom
  show (RawChar char) = "'" <> [char] <> "'"
  show (RawInt int) = show int
  show (RawList expressions) =
    "(" <> intercalate " " (map show expressions) <> ")"
  show (RawString string) = "\"" <> string <> "\""

data SugaredAST
  = SugaredApplication SugaredAST
                       [SugaredAST]
  | SugaredAtom Atom
  | SugaredChar Char
  | SugaredInt Int
  | SugaredLambda [Atom]
                  SugaredAST
  | SugaredLet [(Atom, SugaredAST)]
               SugaredAST
  | SugaredString String

instance Show SugaredAST where
  show :: SugaredAST -> String
  show (SugaredApplication fn args) =
    "(" <> intercalate " " (show fn : map show args) <> ")"
  show (SugaredAtom atom) = atom
  show (SugaredChar char) = "'" <> [char] <> "'"
  show (SugaredInt int) = show int
  show (SugaredLambda params body) =
    "(λ (" <> intercalate " " params <> ") " <> show body <> ")"
  show (SugaredLet defs body) =
    let showDef (var, val) = "(" <> intercalate " " [var, show val] <> ")"
    in "(let (" <> intercalate " " (map showDef defs) <> ") " <> show body <>
       ")"
  show (SugaredString string) = "\"" <> string <> "\""

data AST
  = Application AST
                [AST]
  | Atom Atom
  | Char' Char
  | Int' Int
  | Lambda Atom
           AST
  | String' String

instance Show AST where
  show :: AST -> String
  show (Application fn args) =
    "(" <> intercalate " " (show fn : map show args) <> ")"
  show (Atom atom) = atom
  show (Char' char) = "'" <> [char] <> "'"
  show (Int' int) = show int
  show (Lambda param body) = show $ Application (Atom "λ") [Atom param, body]
  show (String' string) = "\"" <> string <> "\""
