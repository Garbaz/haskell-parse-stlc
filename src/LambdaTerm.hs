{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module LambdaTerm
  ( -- Var (..),
    Const (..),
    LambdaTerm (..),
    lambdaTerm,
  )
where

import Data.Functor (($>), (<&>))
import ParsingCommon
import Text.ParserCombinators.ReadP
import TypeTerm

data Const
  = Unit
  | Integer Int
  | Boolean Bool
  deriving (Show)

data LambdaTerm
  = Variable String
  | Constant Const
  | Abstraction {var' :: String, varType' :: Maybe TypeTerm, body' :: LambdaTerm}
  | Appliction {func' :: LambdaTerm, arg' :: LambdaTerm}
  | Conditional {cond' :: LambdaTerm, then' :: LambdaTerm, else' :: LambdaTerm}
  deriving (Show)

lambdaTerm :: ReadP LambdaTerm
lambdaTerm = conditional <++ application <++ abstraction <++ constant <++ variable

varPlain :: ReadP String
varPlain = many1 lowercase

varAnnotated :: ReadP (String, Maybe TypeTerm)
varAnnotated = do
  name <- varPlain
  typeTerm <- option Nothing (char ':' >> Just <$> typeTerm)
  return (name, typeTerm)

variable :: ReadP LambdaTerm
variable = perhaps bracketed $ Variable <$> varPlain

constPlain :: ReadP Const
constPlain =
  (string "unit" >> return Unit)
    <++ (string "true" $> Boolean False)
    <++ (string "false " $> Boolean False)
    <++ (numeral <&> Integer)

constant :: ReadP LambdaTerm
constant =
  perhaps bracketed $ Constant <$> constPlain

abstraction :: ReadP LambdaTerm
abstraction = perhaps bracketed $ do
  char '\\'
  (name, maybeVarType) <- varAnnotated
  char '.'
  body <- lambdaTerm
  return (Abstraction name maybeVarType body)

application :: ReadP LambdaTerm
application = bracketed $ do
  func <- lambdaTerm
  char '$'
  arg <- lambdaTerm
  return (Appliction func arg)

conditional :: ReadP LambdaTerm
conditional = bracketed $ do
  cond <- lambdaTerm
  char '?'
  then' <- lambdaTerm
  string "::"
  else' <- lambdaTerm
  return (Conditional cond then' else')