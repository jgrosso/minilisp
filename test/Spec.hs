{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import Data.Semigroup ((<>))

import Minilisp (minilisp)
import Minilisp.AST
       (AST(Application, Atom, Char', Int', Lambda, List),
        RawAST(RawAtom, RawChar, RawInt, RawList, RawQuotedList),
        SugaredAST(SugaredApplication, SugaredAtom,
                   SugaredChar, SugaredInt, SugaredLambda, SugaredLet, SugaredList))
import Minilisp.App (AppM, evalAppM)
import Minilisp.Parse
       (atom, char', expression, int, list, quotedList, sExp,
        string', whitespace)
import Minilisp.Stages (desugarAST, normalizeAST)
import Minilisp.Terminal as Terminal (configure)

import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Char (alphaNum)
import Text.Parsec.Combinator (eof)

main :: IO ()
main = do
  Terminal.configure
  tests

tests :: IO ()
tests = do
  describe "Parse" $ do
    let (parser, input) <== expected =
          case Parsec.parse (parser <* eof) "" input of
            Right result ->
              putStrLn $
              if expected == result
                then "SUCCESS"
                else "ERROR: " <> show expected <> " /= " <> show result
            Left err -> putStrLn $ "ERROR: " <> show err
    describe "Combinators" $
      it "whitespace" $ do
        (whitespace, "") <== ""
        (whitespace, " \t\n\r") <== " \t\n\r"
    describe "Grammar" $ do
      it "sExp" $
        (sExp, "( a b c d )") <==
        [RawAtom "a", RawAtom "b", RawAtom "c", RawAtom "d"]
      it "atom" $ (atom, "a!1") <== RawAtom "a!1"
      it "char'" $ (char', "'a'") <== RawChar 'a'
      it "expression" $ do
        (expression, "a!1") <== RawAtom "a!1"
        (expression, "'a'") <== RawChar 'a'
        (expression, "123") <== RawInt 123
        (expression, "( a b c d )") <==
          RawList [RawAtom "a", RawAtom "b", RawAtom "c", RawAtom "d"]
        (expression, "'( a b c d )") <==
          RawQuotedList [RawAtom "a", RawAtom "b", RawAtom "c", RawAtom "d"]
        (expression, "\"asdf\"") <==
          RawQuotedList [RawChar 'a', RawChar 's', RawChar 'd', RawChar 'f']
      it "int" $ (int, "123") <== RawInt 123
      it "quotedList" $ do
        (quotedList, "'()") <== RawQuotedList []
        (quotedList, "'( a b c d )") <==
          RawQuotedList [RawAtom "a", RawAtom "b", RawAtom "c", RawAtom "d"]
      it "string'" $ do
        (string', "\"\"") <== RawQuotedList []
        (string', "\"asdf\"") <==
          RawQuotedList [RawChar 'a', RawChar 's', RawChar 'd', RawChar 'f']
  describe "Stages" $ do
    let a <==> b =
          case map fst $ [evalAppM a, evalAppM b] of
            [Right aValue, Right bValue] ->
              putStrLn $
              if aValue == bValue
                then "SUCCESS"
                else "ERROR: " <> show aValue <> " /= " <> show bValue
            [Left err, _] -> putStrLn $ "ERROR: " <> show err
            [_, Left err] -> putStrLn $ "ERROR: " <> show err
    let a <== b = a <==> return b
    let a ==> b = return a <==> b
    describe "normalizeAST" $ do
      it "correctly normalizes atoms" $
        normalizeAST (RawAtom "a") <== SugaredAtom "a"
      it "correctly normalizes chars" $
        normalizeAST (RawChar 'a') <== SugaredChar 'a'
      it "correctly normalizes ints" $ normalizeAST (RawInt 1) <== SugaredInt 1
      it "correctly normalizes lambdas" $
        let params = ["a", "b"]
            body = RawAtom "body"
        in normalizeAST
             (RawList [RawAtom "lambda", RawList (map RawAtom params), body]) <==>
           (SugaredLambda params <$> normalizeAST body)
      it "correctly normalizes lets" $
        let defA = RawAtom "defA"
            defB = RawAtom "defB"
            body = RawAtom "body"
        in normalizeAST
             (RawList
                [ RawAtom "let"
                , RawList
                    [RawList [RawAtom "a", defA], RawList [RawAtom "b", defB]]
                , body
                ]) <==> do
             defA' <- normalizeAST defA
             defB' <- normalizeAST defB
             body' <- normalizeAST body
             pure $ SugaredLet [("a", defA'), ("b", defB')] body'
      it "correctly normalizes applications" $
        let fn = RawAtom "fn"
            args = [RawAtom "a", RawAtom "b"]
        in normalizeAST (RawList (fn : args)) <==>
           (SugaredApplication <$> normalizeAST fn <*>
            traverse normalizeAST args)
      it "correctly normalizes quoted lists" $
        let rawItems = [RawAtom "a", RawAtom "b"]
        in normalizeAST (RawQuotedList rawItems) <==>
           (SugaredList <$> traverse normalizeAST rawItems)
    describe "desugarAST" $ do
      it "correctly desugars applications" $
        let fn = SugaredAtom "fn"
            args = [SugaredAtom "a", SugaredAtom "b"]
        in desugarAST (SugaredApplication fn args) <==>
           (Application <$> desugarAST fn <*> traverse desugarAST args)
      it "correctly desugars atoms" $ desugarAST (SugaredAtom "a") <== Atom "a"
      it "correctly desugars chars" $ desugarAST (SugaredChar 'a') <== Char' 'a'
      it "correctly desugars ints" $ desugarAST (SugaredInt 1) <== Int' 1
      it "correctly desugars lambdas" $
        let body = SugaredAtom "body"
        in desugarAST (SugaredLambda ["a", "b"] body) <==> do
             body' <- desugarAST body
             pure $ Lambda "a" (Lambda "b" body')
      it "correctly desugars lets" $
        let defA = SugaredAtom "a"
            defB = SugaredAtom "b"
            body = SugaredAtom "body"
        in desugarAST (SugaredLet [("a", defA), ("b", defB)] body) <==> do
             defA' <- desugarAST defA
             defB' <- desugarAST defB
             body' <- desugarAST body
             pure $ Application (Lambda "a" (Lambda "b" body')) [defA', defB']
      it "correctly desugars lists" $
        let items = [SugaredAtom "a", SugaredAtom "b"]
        in desugarAST (SugaredList items) <==>
           (List <$> traverse desugarAST items)
  where
    describe name = (putStrLn ("====== " <> name <> " ======") >>)
    it name = (putStrLn name >>)
