{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module TypeTerm
  ( TypeTerm (..),
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

data TypeTerm
  = TypeVariable String
  | TypeConstant BaseType
  | TypeFunction {from' :: TypeTerm, to' :: TypeTerm}
  deriving (Show)

typeTerm :: ReadP TypeTerm
typeTerm = typeVariable <|> typeConstant <|> functionType

typeVariable :: ReadP TypeTerm
typeVariable = perhaps bracketed $ TypeVariable <$> capitalized

typeConstant :: ReadP TypeTerm
typeConstant =
  perhaps bracketed $
    TypeConstant
      <$> ( (string "Unit" >> return UnitType)
              <++ (string "Bool" >> return BooleanType)
              <++ (string "Int" >> return IntegerType)
          )

functionType :: ReadP TypeTerm
functionType = bracketed $ do
  from <- typeTerm
  string "->"
  to <- typeTerm
  return (TypeFunction from to)