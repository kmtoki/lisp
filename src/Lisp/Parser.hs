{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict, StrictData #-}

module Lisp.Parser where

import Lisp.AST
import Control.Applicative
import qualified Text.Parsec as P

parse :: String -> Either P.ParseError AST
parse = P.parse sexpr ""

-- top = List <$> pos <*> (P.many (sexpr <* spaces) <* P.eof)

sexpr = atom <|> list

pos = ((,) <$> P.sourceLine <*> P.sourceColumn) <$> P.getPosition

spaces = P.skipMany (P.space <|> P.tab <|> P.newline <|> comment)

comment = P.string ";" *> P.noneOf "\n" <* P.newline

atom 
  = Atom <$> pos <*> P.many1 (P.noneOf "0123456789()' \t\r\n") 
  <|> Nat <$> pos <*> (read <$> P.many1 P.digit)

list = P.string "(" *> (List <$> pos <*> P.many (sexpr <* spaces)) <* P.string ")"
