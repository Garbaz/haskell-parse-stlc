{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module LambdaTerm
  ( Var,
    LambdaTerm,
    lambdaTerm,
    TypeVar,
    TypeTerm,
  )
where

import ParsingCommon
import Text.ParserCombinators.ReadP
import TypeTerm

data Var = Var {name :: String, maybeTypeTerm :: Maybe TypeTerm}
  deriving (Show)

data LambdaTerm
  = Variable {var :: Var}
  | Abstraction {var :: Var, body :: LambdaTerm}
  | Appliction {func :: LambdaTerm, arg :: LambdaTerm}
  deriving (Show)

lambdaTerm :: ReadP LambdaTerm
lambdaTerm = application <++ abstraction <++ variable

varPlain :: ReadP Var
varPlain = do
  name <- many1 lowerCase
  return (Var name Nothing)

varAnnotated :: ReadP Var
varAnnotated = do
  varPlain <- varPlain
  maybeTypeTerm <- option Nothing (char ':' >> Just <$> typeTerm)
  return (Var (name varPlain) maybeTypeTerm)

variable :: ReadP LambdaTerm
variable = perhaps bracketed $ Variable <$> varPlain

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