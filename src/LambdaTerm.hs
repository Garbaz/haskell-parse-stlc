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
  | Id
  | Cond

instance Show Const where
  show Unit = "unit"
  show (Integer i) = show i
  show (Boolean True) = "true"
  show (Boolean False) = "false"
  show Addition = "add"
  show Multiplication = "mul"
  show LessThan = "lt"
  show Or = "or"
  show And = "and"
  show Not = "not"
  show Id = "id"
  show Cond = "cond"

data LambdaExpr
  = Variable String
  | Constant Const
  | Abstraction {argVar' :: String, argType :: TypeExpr, body' :: LambdaExpr}
  | Application {func' :: LambdaExpr, arg' :: LambdaTerm}
  | Let {letVar' :: String, letExpr' :: LambdaExpr, letBody' :: LambdaExpr}

-- | Conditional {cond' :: LambdaExpr, then' :: LambdaExpr, else' :: LambdaExpr}
instance Show LambdaExpr where
  show (Variable v) = v
  show (Constant c) = show c
  show (Abstraction v t b) = "(\\" ++ v ++ " : " ++ show t ++ " . " ++ show b ++ ")"
  show (Application f a) = "(" ++ show f ++ " $ " ++ show a ++ ")"
  show (Let v e b) = "[" ++ v ++ " := " ++ show e ++ "]" ++ show b

-- show (Conditional c t e) = "(" ++ show c ++ "?" ++ show t ++ "::" ++ show e ++ ")"

data LambdaTerm = LambdaTerm {lambdaTypeTag' :: Maybe String, lambdaExpr' :: LambdaExpr}

instance Show LambdaTerm where
  show (LambdaTerm Nothing e) = show e
  show (LambdaTerm (Just t) e) = t ++ "=" ++ show e

-- data PolyLambda = PolyLambda {polyVars' :: [String], lambdaTerm' :: LambdaTerm}

-- instance Show PolyLambda where
--   show (PolyLambda _ t) = show t

-- polyLambda :: ReadP PolyLambda
-- polyLambda = undefined

lambdaTerm :: ReadP LambdaTerm
lambdaTerm = taggedLambdaTerm <|> untaggedLambdaTerm

taggedLambdaTerm :: ReadP LambdaTerm
taggedLambdaTerm = do
  tag <- many1 lowercase
  char '='
  lambdaExpr <- lambdaExpr
  return (LambdaTerm (Just tag) lambdaExpr)

untaggedLambdaTerm :: ReadP LambdaTerm
untaggedLambdaTerm = LambdaTerm Nothing <$> lambdaExpr

lambdaExpr :: ReadP LambdaExpr
lambdaExpr = variable <|> constant <|> abstraction <|> let' <|> application

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
    <|> string "lt" $> LessThan
    <|> string "or" $> Or
    <|> string "and" $> And
    <|> string "not" $> Not
    <|> string "cond" $> Cond
    <|> string "id" $> Id

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

let' :: ReadP LambdaExpr
let' = do
  char '['
  letVar <- varPlain
  string ":="
  letExpr <- lambdaExpr
  char ']'
  letBody <- lambdaExpr
  return (Let letVar letExpr letBody)

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

-- conditional :: ReadP LambdaExpr
-- conditional = bracketed $ do
--   cond <- lambdaExpr
--   char '?'
--   then' <- lambdaExpr
--   string "::"
--   Conditional cond then' <$> lambdaExpr

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
    (TypeTerm (Just "x") (TypeConstant IntegerType))
    ( TypeFunction
        (TypeTerm (Just "y") (TypeConstant IntegerType))
        (TypeConstant BooleanType)
    )
typeOfConst Or =
  TypeFunction
    (TypeTerm (Just "x") (TypeConstant BooleanType))
    ( TypeFunction
        (TypeTerm (Just "y") (TypeConstant BooleanType))
        (TypeConstant BooleanType)
    )
typeOfConst And =
  TypeFunction
    (TypeTerm (Just "x") (TypeConstant BooleanType))
    ( TypeFunction
        (TypeTerm (Just "y") (TypeConstant BooleanType))
        (TypeConstant BooleanType)
    )
typeOfConst Not =
  TypeFunction
    (TypeTerm (Just "x") (TypeConstant BooleanType))
    (TypeConstant BooleanType)
typeOfConst Id =
  TypeFunction
    (TypeTerm (Just "x") (TypeVariable "a"))
    (TypeVariable "a")
typeOfConst Cond =
  TypeFunction
    (TypeTerm (Just "if") (TypeConstant BooleanType))
    ( TypeFunction
        (TypeTerm (Just "then") (TypeVariable "a"))
        ( TypeFunction
            (TypeTerm (Just "else") (TypeVariable "a"))
            (TypeVariable "a")
        )
    )