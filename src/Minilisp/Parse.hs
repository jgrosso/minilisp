{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minilisp.Parse
  ( atom
  , char'
  , expression
  , identifier
  , int
  , list
  , many
  , parse
  , program
  , quotedAtom
  , quotedList
  , sExp
  , string'
  , whitespace
  ) where

import Control.Monad.Except (MonadError, throwError)

import Minilisp.AST
       (RawAST(RawAtom, RawChar, RawInt, RawList, RawQuotedAtom,
               RawQuotedList))
import Minilisp.Error (Error(Error), Type(InvalidSyntax))

import Text.Parsec ((<|>), eof, Stream, ParsecT, try)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Char
       (alphaNum, char, digit, letter, noneOf, oneOf, space)
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
atom = RawAtom <$> identifier

char'
  :: Stream s m Char
  => ParsecT s u m RawAST
char' = RawChar <$> (char '\'' *> noneOf "\'" <* char '\'')

expression
  :: Stream s m Char
  => ParsecT s u m RawAST
expression =
  atom <|> char' <|> int <|> list <|> try quotedAtom <|> quotedList <|> string'

identifier
  :: Stream s m Char
  => ParsecT s u m String
identifier = (:) <$> (letter <|> symbol) <*> many (alphaNum <|> symbol)
  where
    symbol = oneOf "!@#$%^&*-=~_+,./<>?\\|"

int
  :: Stream s m Char
  => ParsecT s u m RawAST
int = RawInt . read <$> many1 digit

list
  :: Stream s m Char
  => ParsecT s u m RawAST
list = RawList <$> sExp

quotedAtom
  :: Stream s m Char
  => ParsecT s u m RawAST
quotedAtom = RawQuotedAtom <$> (char '`' *> identifier)

quotedList
  :: Stream s m Char
  => ParsecT s u m RawAST
quotedList = RawQuotedList <$> (char '`' *> sExp)

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
