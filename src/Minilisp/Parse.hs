{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minilisp.Parse
  ( parse
  ) where

import Control.Monad.Except (MonadError, throwError)

import Data.Semigroup ((<>))

import Minilisp.AST
       (RawAST(RawAtom, RawChar, RawInt, RawList, RawString))
import Minilisp.Error (Error(Error), Type(InvalidSyntax))

import Text.Parsec ((<|>), Stream, ParseError, ParsecT, try)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Char
       (alphaNum, char, digit, letter, noneOf, oneOf, space)
import Text.Parsec.Combinator (many1, option, optional)

many
  :: Stream s m Char
  => ParsecT s u m a -> ParsecT s u m [a]
many = option mempty . many1

whitespace
  :: Stream s m Char
  => ParsecT s u m String
whitespace = many space

atom
  :: Stream s m Char
  => ParsecT s u m RawAST
atom = RawAtom <$> ((:) <$> (letter <|> symbol) <*> many (alphaNum <|> symbol))
  where
    symbol = oneOf "`!@#$%^&*-=~_+`,./<>?"

list
  :: Stream s m Char
  => ParsecT s u m RawAST
list = RawList <$> (char '(' *> many1 item <* optional whitespace <* char ')')
  where
    item = optional whitespace *> expression

string
  :: Stream s m Char
  => ParsecT s u m RawAST
string = RawString <$> (char '"' *> many1 (noneOf "\"") <* char '"')

char'
  :: Stream s m Char
  => ParsecT s u m RawAST
char' = RawChar <$> (char '\'' *> noneOf "\'" <* char '\'')

int'
  :: Stream s m Char
  => ParsecT s u m RawAST
int' = RawInt <$> read <$> many1 digit

expression
  :: Stream s m Char
  => ParsecT s u m RawAST
expression = atom <|> char' <|> int' <|> list <|> string

parse
  :: (MonadError Error m)
  => String -> m RawAST
parse input =
  case Parsec.parse expression "" input of
    Right result -> return result
    Left err -> throwError $ Error (InvalidSyntax (show err)) Nothing
