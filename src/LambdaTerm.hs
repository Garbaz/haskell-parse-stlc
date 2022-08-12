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
  | LessThan
  | Or
  | And
  | Not

instance Show Const where
  show Unit = "unit"
  show (Integer i) = show i
  show (Boolean True) = "true"
  show (Boolean False) = "false"
  show Addition  = "add"
  show Multiplication = "mul"
  show LessThan = "lt"
  show Or = "or"
  show And = "and"
  show Not = "not"

data LambdaExpr
  = Variable String
  | Constant Const
  | Abstraction {argVar' :: String, argType :: TypeExpr, body' :: LambdaExpr}
  | Application {func' :: LambdaExpr, arg' :: LambdaTerm}
  | Conditional {cond' :: LambdaExpr, then' :: LambdaExpr, else' :: LambdaExpr}

instance Show LambdaExpr where
  show (Variable v) = v
  show (Constant c) = show c
  show (Abstraction v t b) = "(\\" ++ v ++ " : " ++ show t ++ " . " ++ show b ++ ")"
  show (Application f a)     = "(" ++ show f ++ " $ " ++ show a ++ ")"
  show (Conditional c t e)   = "(" ++ show c ++ "?" ++ show t ++ "::" ++ show e ++ ")"

data LambdaTerm = LambdaTerm {lambdaTypeTag' :: Maybe String, lambdaExpr' :: LambdaExpr}

instance Show LambdaTerm where
  show (LambdaTerm Nothing e) = show e
  show (LambdaTerm (Just t) e) = t ++ "=" ++ show e

lambdaExpr :: ReadP LambdaExpr
lambdaExpr = variable <|> constant <|> abstraction <|> application <|> conditional

lambdaTerm :: ReadP LambdaTerm
lambdaTerm = taggedLambdaTerm <|> untaggedLambdaTerm

taggedLambdaTerm :: ReadP LambdaTerm
taggedLambdaTerm = do
  tag <- many1 lowercase
  char '='
  LambdaTerm (Just tag) <$> lambdaExpr

untaggedLambdaTerm :: ReadP LambdaTerm
untaggedLambdaTerm = LambdaTerm Nothing <$> lambdaExpr

varPlain :: ReadP String
varPlain = many1 lowercase

varAnnotated :: ReadP (String, TypeExpr)
varAnnotated = do
  name <- varPlain
  typeExpr <- char ':' >> typeExpr
  return (name, typeExpr)

variable :: ReadP LambdaExpr
variable = perhaps bracketed $ Variable <$> varPlain

constPlain :: ReadP Const
constPlain =
  string "unit" $> Unit
    <|> string "true" $> Boolean True
    <|> string "false" $> Boolean False
    <|> (numeral <&> Integer)
    <|> string "add" $> Addition
    <|> string "mul" $> Multiplication
    <|> string "or" $> Or
    <|> string "and" $> And
    <|> string "not" $> Not
    <|> string "lt" $> LessThan

constant :: ReadP LambdaExpr
constant = perhaps bracketed $ Constant <$> constPlain

-- argsTerm :: ReadP [(String, TypeTerm)]
-- argsTerm = sepBy varAnnotated (char ',')

abstraction :: ReadP LambdaExpr
abstraction = perhaps bracketed $ do
  char '\\'
  (arg, argType) <- varAnnotated
  char '.'
  Abstraction arg argType <$> lambdaExpr

-- application :: ReadP LambdaExpr
-- application = bracketed $ do
--   func <- lambdaExpr
--   char '$'
--   arg <- lambdaExpr
--   return (Application func arg)

application :: ReadP LambdaExpr
application = bracketed $ do
  ((LambdaTerm Nothing func) : args) <- sepBy1 lambdaTerm (char '$')
  applMultiArgs func args
  where
    applMultiArgs :: LambdaExpr -> [LambdaTerm] -> ReadP LambdaExpr
    applMultiArgs f [a] = return (Application f a)
    applMultiArgs f (a : as) = applMultiArgs (Application f a) as
    applMultiArgs _ _ = pfail

conditional :: ReadP LambdaExpr
conditional = bracketed $ do
  cond <- lambdaExpr
  char '?'
  then' <- lambdaExpr
  string "::"
  Conditional cond then' <$> lambdaExpr

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
typeOfConst LessThan =
  TypeFunction
    (TypeTerm (Just "l") (TypeConstant IntegerType))
    ( TypeFunction
        (TypeTerm (Just "r") (TypeConstant IntegerType))
        (TypeConstant BooleanType)
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
