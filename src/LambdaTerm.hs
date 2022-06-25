{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module LambdaTerm
  ( -- Var (..),
    Const (..),
    LambdaTerm (..),
    lambdaTerm,
    constant,
  )
where

import Control.Applicative ((<|>))
import Data.Functor (($>), (<&>))
import ParsingCommon
import Text.ParserCombinators.ReadP
import TypeTerm

data Const
  = Unit
  | Integer Int
  | Boolean Bool
  | Addition
  | Multiplication
  | Or
  | And
  | Not
  deriving (Show)

typeOf :: Const -> TypeExpr
typeOf Unit = TypeConstant UnitType
typeOf (Integer _) = TypeConstant IntegerType
typeOf (Boolean _) = TypeConstant BooleanType
typeOf Addition =
  TypeFunction
    (TypeTerm Nothing (TypeConstant IntegerType))
    ( TypeTerm
        Nothing
        ( TypeFunction
            (TypeTerm Nothing (TypeConstant IntegerType))
            (TypeTerm Nothing (TypeConstant IntegerType))
        )
    )
typeOf Multiplication =
  TypeFunction
    (TypeTerm Nothing (TypeConstant IntegerType))
    ( TypeTerm
        Nothing
        ( TypeFunction
            (TypeTerm Nothing (TypeConstant IntegerType))
            (TypeTerm Nothing (TypeConstant IntegerType))
        )
    )
typeOf Or =
  TypeFunction
    (TypeTerm Nothing (TypeConstant BooleanType))
    ( TypeTerm
        Nothing
        ( TypeFunction
            (TypeTerm Nothing (TypeConstant BooleanType))
            (TypeTerm Nothing (TypeConstant BooleanType))
        )
    )
typeOf And =
  TypeFunction
    (TypeTerm Nothing (TypeConstant BooleanType))
    ( TypeTerm
        Nothing
        ( TypeFunction
            (TypeTerm Nothing (TypeConstant BooleanType))
            (TypeTerm Nothing (TypeConstant BooleanType))
        )
    )
typeOf Not =
  TypeFunction
    (TypeTerm Nothing (TypeConstant BooleanType))
    (TypeTerm Nothing (TypeConstant BooleanType))

data LambdaTerm
  = Variable String
  | Constant Const
  | Abstraction {var' :: String, varType' :: TypeTerm, body' :: LambdaTerm}
  | Application {func' :: LambdaTerm, arg' :: LambdaTerm}
  | Conditional {cond' :: LambdaTerm, then' :: LambdaTerm, else' :: LambdaTerm}
  deriving (Show)

lambdaTerm :: ReadP LambdaTerm
lambdaTerm = variable <|> constant <|> abstraction <|> application <|> conditional

varPlain :: ReadP String
varPlain = many1 lowercase

varAnnotated :: ReadP (String, TypeTerm)
varAnnotated = do
  name <- varPlain
  typeTerm <- char ':' >> typeTerm
  return (name, typeTerm)

variable :: ReadP LambdaTerm
variable = perhaps bracketed $ Variable <$> varPlain

constPlain :: ReadP Const
constPlain =
  (string "unit" $> Unit)
    <|> (string "true" $> Boolean True)
    <|> (string "false" $> Boolean False)
    <|> (numeral <&> Integer)
    <|> (string "add" $> Addition)
    <|> (string "mul" $> Multiplication)
    <|> (string "or" $> Or)
    <|> (string "and" $> And)
    <|> (string "not" $> Not)

constant :: ReadP LambdaTerm
constant = perhaps bracketed $ Constant <$> constPlain

abstraction :: ReadP LambdaTerm
abstraction = perhaps bracketed $ do
  char '\\'
  (varName, varType) <- varAnnotated
  char '.'
  body <- lambdaTerm
  return (Abstraction varName varType body)

application :: ReadP LambdaTerm
application = bracketed $ do
  func <- lambdaTerm
  char '$'
  arg <- lambdaTerm
  return (Application func arg)

conditional :: ReadP LambdaTerm
conditional = bracketed $ do
  cond <- lambdaTerm
  char '?'
  then' <- lambdaTerm
  string "::"
  else' <- lambdaTerm
  return (Conditional cond then' else')