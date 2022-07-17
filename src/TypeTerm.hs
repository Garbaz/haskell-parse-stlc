{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module TypeTerm
  ( BaseType (..),
    TypeExpr (..),
    TypeTerm (..),
    typeExpr,
    typeTerm,
  )
where

import Control.Applicative ((<|>))
import ParsingCommon
import Text.ParserCombinators.ReadP

data BaseType
  = UnitType
  | BooleanType
  | IntegerType
  deriving (Show, Eq)

data TypeExpr
  = TypeConstant BaseType
  | TypeFunction {from' :: TypeTerm, to' :: TypeExpr}
  deriving (Show, Eq)

data TypeTerm = TypeTerm {typeTag' :: Maybe String, typeExpr' :: TypeExpr}
  deriving (Show, Eq)

typeExpr :: ReadP TypeExpr
-- typeExpr = typeVariable <|> typeConstant <|> functionType
typeExpr = typeConstant <|> functionType

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

-- typeVariable :: ReadP TypeExpr
-- typeVariable = perhaps bracketed $ TypeVariable <$> capitalized

typeConstant :: ReadP TypeExpr
typeConstant =
  perhaps bracketed $
    TypeConstant
      <$> ( (string "Unit" >> return UnitType)
              <|> (string "Bool" >> return BooleanType)
              <|> (string "Int" >> return IntegerType)
          )

functionType :: ReadP TypeExpr
functionType = bracketed $ do
  (from : tos) <- sepBy1 typeTerm (string "->")
  funcMultiTos from tos
  where
    funcMultiTos :: TypeTerm -> [TypeTerm] -> ReadP TypeExpr
    funcMultiTos f [TypeTerm _ a] = return (TypeFunction f a)
    funcMultiTos f (a : as) = TypeFunction f <$> funcMultiTos a as
    funcMultiTos _ _ = pfail