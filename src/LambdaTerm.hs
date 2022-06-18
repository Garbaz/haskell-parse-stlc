{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module LambdaTerm
  ( Var (..),
    Const (..),
    LambdaTerm (..),
    lambdaTerm,
  )
where

import Data.Functor (($>), (<&>))
import ParsingCommon
import Text.ParserCombinators.ReadP
import TypeTerm

data Var = Var {name' :: String, maybeType' :: Maybe TypeTerm}
  deriving (Show)

data Const
  = Unit
  | Integer Int
  | Boolean Bool
  deriving (Show)

data LambdaTerm
  = Variable Var
  | Constant Const
  | Abstraction {var' :: Var, body' :: LambdaTerm}
  | Appliction {func' :: LambdaTerm, arg' :: LambdaTerm}
  | Conditional {cond' :: LambdaTerm, then' :: LambdaTerm, else' :: LambdaTerm}
  deriving (Show)

lambdaTerm :: ReadP LambdaTerm
lambdaTerm = conditional <++ application <++ abstraction <++ constant <++ variable

varPlain :: ReadP Var
varPlain = do
  name <- many1 lowercase
  return (Var name Nothing)

varAnnotated :: ReadP Var
varAnnotated = do
  varPlain <- varPlain
  maybeTypeTerm <- option Nothing (char ':' >> Just <$> typeTerm)
  return (Var (name' varPlain) maybeTypeTerm)

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
abstraction = bracketed $ do
  char '\\'
  var <- varAnnotated
  char '.'
  body <- lambdaTerm
  return (Abstraction var body)

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