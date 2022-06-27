{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module LambdaTerm
  ( Const (..),
    LambdaTerm (..),
    LambdaExpr (..),
    lambdaExpr,
    constant,
    typeOfConst,
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

data LambdaExpr
  = Variable String
  | Constant Const
  | Abstraction {params' :: [(String, TypeTerm)], body' :: LambdaExpr}
  | Application {func' :: LambdaExpr, args' :: [LambdaTerm]}
  | Conditional {cond' :: LambdaExpr, then' :: LambdaExpr, else' :: LambdaExpr}
  deriving (Show)

data LambdaTerm = LambdaTerm {lambdaTypeTag' :: Maybe String, lambdaExpr' :: LambdaExpr}
  deriving (Show)

lambdaExpr :: ReadP LambdaExpr
lambdaExpr = variable <|> constant <|> abstraction <|> application <|> conditional

lambdaTerm :: ReadP LambdaTerm
lambdaTerm = taggedLambdaTerm <|> untaggedLambdaTerm

taggedLambdaTerm :: ReadP LambdaTerm
taggedLambdaTerm = do
  tag <- many1 lowercase
  char '\''
  expr <- lambdaExpr
  return (LambdaTerm (Just tag) expr)

untaggedLambdaTerm :: ReadP LambdaTerm
untaggedLambdaTerm = LambdaTerm Nothing <$> lambdaExpr

varPlain :: ReadP String
varPlain = many1 lowercase

varAnnotated :: ReadP (String, TypeTerm)
varAnnotated = do
  name <- varPlain
  typeTerm <- char ':' >> typeTerm
  return (name, typeTerm)

variable :: ReadP LambdaExpr
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

constant :: ReadP LambdaExpr
constant = perhaps bracketed $ Constant <$> constPlain

argsTerm :: ReadP [(String, TypeTerm)]
argsTerm = sepBy varAnnotated (char ',')

abstraction :: ReadP LambdaExpr
abstraction = perhaps bracketed $ do
  char '\\'
  args <- argsTerm
  char '.'
  body <- lambdaExpr
  return (Abstraction args body)

application :: ReadP LambdaExpr
application = bracketed $ do
  func <- lambdaExpr
  char '$'
  args <- sepBy lambdaTerm (char ',')
  return (Application func args)

conditional :: ReadP LambdaExpr
conditional = bracketed $ do
  cond <- lambdaExpr
  char '?'
  then' <- lambdaExpr
  string "::"
  else' <- lambdaExpr
  return (Conditional cond then' else')

--------------------------------------------------
-------- Predefined type of `Const` terms --------
--------------------------------------------------

typeOfConst :: Const -> TypeExpr
typeOfConst Unit = TypeConstant UnitType
typeOfConst (Integer _) = TypeConstant IntegerType
typeOfConst (Boolean _) = TypeConstant BooleanType
typeOfConst Addition =
  TypeFunction
    [TypeTerm (Just "x") (TypeConstant IntegerType), TypeTerm (Just "y") (TypeConstant IntegerType)]
    (TypeConstant IntegerType)
typeOfConst Multiplication =
  TypeFunction
    [TypeTerm (Just "x") (TypeConstant IntegerType), TypeTerm (Just "y") (TypeConstant IntegerType)]
    (TypeConstant IntegerType)
typeOfConst Or =
  TypeFunction
    [TypeTerm (Just "x") (TypeConstant BooleanType), TypeTerm (Just "y") (TypeConstant BooleanType)]
    (TypeConstant BooleanType)
typeOfConst And =
  TypeFunction
    [TypeTerm (Just "x") (TypeConstant BooleanType), TypeTerm (Just "y") (TypeConstant BooleanType)]
    (TypeConstant BooleanType)
typeOfConst Not =
  TypeFunction
    [TypeTerm (Just "x") (TypeConstant BooleanType)]
    (TypeConstant BooleanType)
