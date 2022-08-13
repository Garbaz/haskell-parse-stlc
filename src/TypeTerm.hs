{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module TypeTerm
  ( BaseType (..),
    TypeExpr (..),
    TypeTerm (..),
    -- TypePoly (..),
    typeExpr,
    typeTerm,
    -- typePoly,
  )
where

import Control.Applicative ((<|>))
import Data.List (intercalate)
import ParsingCommon
import Text.ParserCombinators.ReadP

data BaseType
  = UnitType
  | BooleanType
  | IntegerType
  | TypeType   -- Special type for type variables in the context
  deriving (Eq)

instance Show BaseType where
  show UnitType = "Unit"
  show BooleanType = "Bool"
  show IntegerType = "Int"
  show TypeType = "Type"

data TypeExpr
  = TypeConstant BaseType
  | TypeVariable String
  | TypeFunction {from' :: TypeTerm, to' :: TypeExpr}
  deriving (Eq)

instance Show TypeExpr where
  show (TypeConstant b) = show b
  show (TypeVariable v) = v
  show (TypeFunction f t) = "(" ++ show f ++ "->" ++ show t ++ ")"

-- data TypePoly = TypePoly {polyVars' :: [String], typeTerm' :: TypeExpr}
--   deriving (Eq)

-- instance Show TypePoly where
--   show (TypePoly vs t) = "(" ++ intercalate "," vs ++ "=>" ++ show t ++ ")"

data TypeTerm = TypeTerm {typeTag' :: Maybe String, typeExpr' :: TypeExpr}
  deriving (Eq)

instance Show TypeTerm where
  show (TypeTerm Nothing e) = show e
  show (TypeTerm (Just t) e) = t ++ "'" ++ show e

-- typePoly :: ReadP TypePoly
-- typePoly = perhaps bracketed (polyTypePoly <|> nonpolyTypePoly)

-- polyTypePoly :: ReadP TypePoly
-- polyTypePoly = do
--   polyVars <- sepBy (many1 lowercase) (char ',')
--   string "=>"
--   typeExpr <- typeExpr
--   return (TypePoly polyVars typeExpr)

-- nonpolyTypePoly :: ReadP TypePoly
-- nonpolyTypePoly = do
--   typeExpr <- typeExpr
--   return (TypePoly [] typeExpr)

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

typeExpr :: ReadP TypeExpr
typeExpr = typeConstant <|> typeVariable <|> functionType

typeConstant :: ReadP TypeExpr
typeConstant =
  perhaps bracketed $
    TypeConstant
      <$> ( (string "Unit" >> return UnitType)
              <|> (string "Bool" >> return BooleanType)
              <|> (string "Int" >> return IntegerType)
          )

typeVariable :: ReadP TypeExpr
typeVariable = perhaps bracketed $ TypeVariable <$> many1 lowercase

functionType :: ReadP TypeExpr
functionType = bracketed $ do
  (from : tos) <- sepBy1 typeTerm (string "->")
  funcMultiTos from tos
  where
    funcMultiTos :: TypeTerm -> [TypeTerm] -> ReadP TypeExpr
    funcMultiTos f [TypeTerm _ a] = return (TypeFunction f a)
    funcMultiTos f (a : as) = TypeFunction f <$> funcMultiTos a as
    funcMultiTos _ _ = pfail