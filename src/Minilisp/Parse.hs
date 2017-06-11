{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minilisp.Parse
  ( atom
  , char'
  , expression
  , int
  , list
  , many
  , parse
  , program
  , quotedList
  , sExp
  , string'
  , whitespace
  ) where

import Control.Monad.Except (MonadError, throwError)

import Data.Semigroup ((<>))

import Minilisp.AST
       (RawAST(RawAtom, RawChar, RawInt, RawList, RawQuotedList))
import Minilisp.Error (Error(Error), Type(InvalidSyntax))

import Text.Parsec ((<|>), eof, Stream, ParseError, ParsecT, try)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Char
       (alphaNum, char, digit, letter, noneOf, oneOf, space, string)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (many)

whitespace
  :: Stream s m Char
  => ParsecT s u m String
whitespace = many space

sExp
  :: Stream s m Char
  => ParsecT s u m [RawAST]
sExp = char '(' *> many item <* whitespace <* char ')'
  where
    item = try (whitespace *> expression) <|> expression

atom
  :: Stream s m Char
  => ParsecT s u m RawAST
atom = RawAtom <$> ((:) <$> (letter <|> symbol) <*> many (alphaNum <|> symbol))
  where
    symbol = oneOf "`!@#$%^&*-=~_+,./<>?"

char'
  :: Stream s m Char
  => ParsecT s u m RawAST
char' = RawChar <$> (char '\'' *> noneOf "\'" <* char '\'')

expression
  :: Stream s m Char
  => ParsecT s u m RawAST
expression =
  try atom <|> try char' <|> int <|> list <|> try quotedList <|> string'

int
  :: Stream s m Char
  => ParsecT s u m RawAST
int = RawInt . read <$> many1 digit

list
  :: Stream s m Char
  => ParsecT s u m RawAST
list = RawList <$> sExp

quotedList
  :: Stream s m Char
  => ParsecT s u m RawAST
quotedList = RawQuotedList <$> (char '\'' *> sExp)

string'
  :: Stream s m Char
  => ParsecT s u m RawAST
string' = RawQuotedList <$> chars
  where
    chars = map RawChar <$> (char '"' *> many (noneOf "\"") <* char '"')

program
  :: Stream s m Char
  => ParsecT s u m RawAST
program = expression <* eof

parse
  :: (MonadError Error m)
  => String -> m RawAST
parse input =
  case Parsec.parse program "" input of
    Right result -> return result
    Left err -> throwError $ Error (InvalidSyntax (show err)) Nothing
