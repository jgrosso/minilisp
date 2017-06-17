{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Minilisp.Stages
  ( desugarAST
  , normalizeAST
  ) where

import Control.Monad.Except (MonadError, throwError)

import Data.List (intercalate)

import Minilisp.AST
       (AST(Application, Atom, Char', Int', Lambda, List, QuotedAtom),
        Atom,
        RawAST(RawAtom, RawChar, RawInt, RawList, RawQuotedAtom,
               RawQuotedList),
        SugaredAST(SugaredApplication, SugaredAtom, SugaredChar,
                   SugaredInt, SugaredLambda, SugaredLet, SugaredList,
                   SugaredQuotedAtom))
import Minilisp.Error
       (Error(Error), Type(EmptyList, InvalidArguments))

normalizeAST
  :: (MonadError Error m)
  => RawAST -> m SugaredAST
normalizeAST (RawAtom atom) = return $ SugaredAtom atom
normalizeAST (RawChar char) = return $ SugaredChar char
normalizeAST (RawInt int) = return $ SugaredInt int
normalizeAST expression@(RawList list) =
  case list of
    RawAtom "lambda":rest ->
      case rest of
        [RawList params, body] ->
          SugaredLambda (map (\(RawAtom atom) -> atom) params) <$>
          normalizeAST body
        _ ->
          throwError $
          Error
            (InvalidArguments
               "lambda"
               "a list of parameters and a body"
               (intercalate ", " $ map show rest))
            (Just $ show expression)
    RawAtom "let":rest ->
      case rest of
        [RawList defs, body] ->
          let getDef def =
                case def of
                  RawList [RawAtom var, val] -> (var, ) <$> normalizeAST val
                  _ ->
                    throwError $
                    Error
                      (InvalidArguments
                         "let"
                         "a list of tuples and a body"
                         (intercalate ", " $ map show rest))
                      (Just $ show expression)
          in SugaredLet <$> traverse getDef defs <*> normalizeAST body
        _ ->
          throwError $
          Error
            (InvalidArguments
               "let"
               "a list of tuples and a body"
               (intercalate ", " $ map show rest))
            (Just $ show expression)
    fn:args ->
      SugaredApplication <$> normalizeAST fn <*> traverse normalizeAST args
    [] -> throwError $ Error EmptyList (Just $ show expression)
normalizeAST (RawQuotedAtom atom) = return $ SugaredQuotedAtom atom
normalizeAST (RawQuotedList expressions) =
  SugaredList <$> traverse normalizeAST expressions

curry' :: [Atom] -> AST -> AST
curry' params body =
  case params of
    param:otherParams ->
      if null otherParams
        then Lambda param body
        else Lambda param (curry' otherParams body)
    _ -> error "THIS SHOULD HAVE BEEN CAUGHT BY `normalizeAST`"

desugarAST
  :: (MonadError Error m)
  => SugaredAST -> m AST
desugarAST (SugaredApplication fn args) =
  Application <$> desugarAST fn <*> traverse desugarAST args
desugarAST (SugaredAtom atom) = return $ Atom atom
desugarAST (SugaredChar char) = return $ Char' char
desugarAST (SugaredInt int) = return $ Int' int
desugarAST expression@(SugaredLambda params body) =
  case params of
    [] ->
      throwError $
      Error
        (InvalidArguments "lambda" "a non-empty list of parameters" "()")
        (Just $ show expression)
    _ -> curry' params <$> desugarAST body
desugarAST (SugaredLet defs body) =
  desugarAST $
  SugaredApplication (SugaredLambda (map fst defs) body) (map snd defs)
desugarAST (SugaredList expressions) = List <$> traverse desugarAST expressions
desugarAST (SugaredQuotedAtom atom) = return $ QuotedAtom atom
