{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module LambdaTerm
  ( Const (..),
    LambdaExpr (..),
    LambdaTerm (..),
    lambdaExpr,
    lambdaTerm,
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
  | Abstraction {argVar' :: String, argType :: TypeTerm, body' :: LambdaExpr}
  | Application {func' :: LambdaExpr, arg' :: LambdaExpr}
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

-- argsTerm :: ReadP [(String, TypeTerm)]
-- argsTerm = sepBy varAnnotated (char ',')

abstraction :: ReadP LambdaExpr
abstraction = perhaps bracketed $ do
  char '\\'
  (arg, argType) <- varAnnotated
  char '.'
  body <- lambdaExpr
  return (Abstraction arg argType body)

-- application :: ReadP LambdaExpr
-- application = bracketed $ do
--   func <- lambdaExpr
--   char '$'
--   arg <- lambdaExpr
--   return (Application func arg)

application :: ReadP LambdaExpr
application = bracketed $ do
  (func : args) <- sepBy1 lambdaExpr (char '$')
  applyMultiArgs func args
  where
    applyMultiArgs :: LambdaExpr -> [LambdaExpr] -> ReadP LambdaExpr
    applyMultiArgs f [a] = return (Application f a)
    applyMultiArgs f (a : as) = applyMultiArgs (Application f a) as
    applyMultiArgs _ _ = pfail

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
    (TypeTerm (Just "x") (TypeConstant IntegerType))
    ( TypeFunction
        (TypeTerm (Just "y") (TypeConstant IntegerType))
        (TypeConstant IntegerType)
    )
typeOfConst Multiplication =
  TypeFunction
    (TypeTerm (Just "x") (TypeConstant IntegerType))
    ( TypeFunction
        (TypeTerm (Just "y") (TypeConstant IntegerType))
        (TypeConstant IntegerType)
    )
typeOfConst Or =
  TypeFunction
    (TypeTerm (Just "a") (TypeConstant BooleanType))
    ( TypeFunction
        (TypeTerm (Just "b") (TypeConstant BooleanType))
        (TypeConstant BooleanType)
    )
typeOfConst And =
  TypeFunction
    (TypeTerm (Just "a") (TypeConstant BooleanType))
    ( TypeFunction
        (TypeTerm (Just "b") (TypeConstant BooleanType))
        (TypeConstant BooleanType)
    )
typeOfConst Not =
  TypeFunction
    (TypeTerm (Just "x") (TypeConstant BooleanType))
    (TypeConstant BooleanType)
