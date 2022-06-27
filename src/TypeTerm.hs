{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module TypeTerm
  ( TypeExpr (..),
    TypeTerm (..),
    typeTerm,
    BaseType (..),
  )
where

import Control.Applicative ((<|>))
import ParsingCommon
import Text.ParserCombinators.ReadP

data BaseType
  = UnitType
  | BooleanType
  | IntegerType
  deriving (Show)

data TypeExpr
  = TypeVariable String
  | TypeConstant BaseType
  | TypeFunction {from' :: TypeTerm, to' :: TypeTerm}
  deriving (Show)

data TypeTerm = TypeTerm {typeTag' :: Maybe String, typeExpr' :: TypeExpr}
  deriving (Show)

typeExpr :: ReadP TypeExpr
typeExpr = typeVariable <|> typeConstant <|> functionType

typeTerm :: ReadP TypeTerm
typeTerm = untaggedTypeTerm <|> taggedTypeTerm

untaggedTypeTerm :: ReadP TypeTerm
untaggedTypeTerm = TypeTerm Nothing <$> typeExpr

taggedTypeTerm :: ReadP TypeTerm
taggedTypeTerm = do
  typeTag <- Just <$> many1 lowercase
  char '\''
  typeExpr <- typeExpr
  return (TypeTerm typeTag typeExpr)

typeVariable :: ReadP TypeExpr
typeVariable = perhaps bracketed $ TypeVariable <$> capitalized

typeConstant :: ReadP TypeExpr
typeConstant =
  perhaps bracketed $
    TypeConstant
      <$> ( (string "Unit" >> return UnitType)
              <++ (string "Bool" >> return BooleanType)
              <++ (string "Int" >> return IntegerType)
          )

functionType :: ReadP TypeExpr
functionType = bracketed $ do
  from <- typeTerm
  string "->"
  to <- typeTerm
  return (TypeFunction from to)