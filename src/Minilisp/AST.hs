{-# LANGUAGE InstanceSigs #-}

module Minilisp.AST
  ( AST(Application, Atom, Char', Int', Lambda, List, QuotedAtom)
  , Atom
  , RawAST(RawAtom, RawChar, RawInt, RawList, RawQuotedAtom,
       RawQuotedList)
  , SugaredAST(SugaredApplication, SugaredAtom, SugaredChar,
           SugaredInt, SugaredLambda, SugaredLet, SugaredList,
           SugaredQuotedAtom)
  ) where

import Data.Semigroup ((<>))

type Atom = String

data RawAST
  = RawAtom Atom
  | RawChar Char
  | RawInt Int
  | RawList [RawAST]
  | RawQuotedAtom Atom
  | RawQuotedList [RawAST]
  deriving (Eq)

instance Show RawAST where
  show :: RawAST -> String
  show (RawAtom atom) = atom
  show (RawChar char) = "'" <> [char] <> "'"
  show (RawInt int) = show int
  show (RawList expressions) = "(" <> unwords (map show expressions) <> ")"
  show (RawQuotedAtom atom) = '`' : atom
  show (RawQuotedList expressions) =
    case traverse extractChar expressions of
      Just string -> "\"" <> string <> "\""
      Nothing -> "`(" <> unwords (map show expressions) <> ")"
    where
      extractChar (RawChar char) = Just char
      extractChar _ = Nothing

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
  | SugaredList [SugaredAST]
  | SugaredQuotedAtom Atom
  deriving (Eq)

instance Show SugaredAST where
  show :: SugaredAST -> String
  show (SugaredApplication fn args) =
    "(" <> unwords (show fn : map show args) <> ")"
  show (SugaredAtom atom) = atom
  show (SugaredChar char) = "'" <> [char] <> "'"
  show (SugaredInt int) = show int
  show (SugaredLambda params body) =
    "(λ (" <> unwords params <> ") " <> show body <> ")"
  show (SugaredLet defs body) =
    let showDef (var, val) = "(" <> unwords [var, show val] <> ")"
    in "(let (" <> unwords (map showDef defs) <> ") " <> show body <> ")"
  show (SugaredList expressions) =
    case traverse extractChar expressions of
      Just string -> "\"" <> string <> "\""
      Nothing -> "`(" <> unwords (map show expressions) <> ")"
    where
      extractChar (SugaredChar char) = Just char
      extractChar _ = Nothing
  show (SugaredQuotedAtom atom) = '`' : atom

data AST
  = Application AST
                [AST]
  | Atom Atom
  | Char' Char
  | Int' Int
  | Lambda Atom
           AST
  | List [AST]
  | QuotedAtom Atom
  deriving (Eq)

instance Show AST where
  show :: AST -> String
  show (Application fn args) = "(" <> unwords (show fn : map show args) <> ")"
  show (Atom atom) = atom
  show (Char' char) = "'" <> [char] <> "'"
  show (Int' int) = show int
  show (Lambda param body) = show $ Application (Atom "λ") [Atom param, body]
  show (List expressions) =
    case traverse extractChar expressions of
      Just string -> "\"" <> string <> "\""
      Nothing -> "`(" <> unwords (map show expressions) <> ")"
    where
      extractChar (Char' char) = Just char
      extractChar _ = Nothing
  show (QuotedAtom atom) = '`' : atom
