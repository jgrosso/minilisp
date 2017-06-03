{-# LANGUAGE InstanceSigs #-}

module Minilisp.AST
  ( AST(Application, Atom, Char', Int', Lambda, List)
  , Atom
  , RawAST(RawAtom, RawChar, RawInt, RawList)
  , SugaredAST(SugaredApplication, SugaredAtom, SugaredChar,
           SugaredInt, SugaredLambda, SugaredLet, SugaredList)
  ) where

import Data.List (intercalate)
import Data.Semigroup ((<>))

type Atom = String

data RawAST
  = RawAtom Atom
  | RawChar Char
  | RawInt Int
  | RawList [RawAST]

instance Show RawAST where
  show :: RawAST -> String
  show (RawAtom atom) = atom
  show (RawChar char) = "'" <> [char] <> "'"
  show (RawInt int) = show int
  show (RawList expressions) =
    case expressions of
      [RawAtom "quote", RawList items] ->
        case traverse extractChar items of
          Just string -> "\"" <> string <> "\""
          Nothing -> "'(" <> intercalate " " (map show items) <> ")"
      _ -> "(" <> intercalate " " (map show expressions) <> ")"
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
  show (SugaredList expressions) =
    case traverse extractChar expressions of
      Just string -> "\"" <> string <> "\""
      Nothing -> "'(" <> intercalate " " (map show expressions) <> ")"
    where
      extractChar (SugaredChar char) = Just char
      extractChar _ = Nothing

data AST
  = Application AST
                [AST]
  | Atom Atom
  | Char' Char
  | Int' Int
  | Lambda Atom
           AST
  | List [AST]

instance Show AST where
  show :: AST -> String
  show (Application fn args) =
    "(" <> intercalate " " (show fn : map show args) <> ")"
  show (Atom atom) = atom
  show (Char' char) = "'" <> [char] <> "'"
  show (Int' int) = show int
  show (Lambda param body) = show $ Application (Atom "λ") [Atom param, body]
  show (List expressions) =
    case traverse extractChar expressions of
      Just string -> "\"" <> string <> "\""
      Nothing -> "'(" <> intercalate " " (map show expressions) <> ")"
    where
      extractChar (Char' char) = Just char
      extractChar _ = Nothing
