module STLCmodIsom
  ( parseSTLC,
    parseSTLCFail,
    parseTypeExpr,
    parseTypeExprFail,
    checkType,
    inferType,
    checkTypeIsom,
    inferTypeIsom,
    -- From LambdaTerm:
    Const (..),
    LambdaTerm (..),
    LambdaExpr (..),
    -- From TypeTerm:
    BaseType (..),
    TypeExpr (..),
    TypeTerm (..),
  )
where

import LambdaTerm
import Text.ParserCombinators.ReadP (readP_to_S)
import TypeCheck
import TypeCheckIsom
import TypeTerm
import TypingCommon (emptyContext)

cleanup :: String -> String
cleanup = filter (`notElem` [' ', '\n', '\t'])

parseSTLC :: String -> Maybe (LambdaExpr, String)
-- ^ Parses a given Simply Typed Lambda Calculus term
--   as specified in the 'README.md' to the AST.
--   Whitespace is entirely ignored.
parseSTLC e = case readP_to_S lambdaExpr (cleanup e) of
  [] -> Nothing
  xs -> Just (last xs)

parseSTLCFail :: String -> LambdaExpr
-- ^ Same as `parseSTLC`, but throws error upon
-- failing to parse input completely.
parseSTLCFail e = case parseSTLC e of
  Nothing -> error "Failed to parse expression!"
  Just (l, "") -> l
  Just (_, r) -> error ("Expression wasn't fully parsed! Remainder: " ++ r)

parseTypeExpr :: String -> Maybe (TypeExpr, String)
-- ^ Parses a given TypeExpr term
--   as specified in the 'README.md' to the Type-AST.
--   Whitespace is entirely ignored.
parseTypeExpr t = case readP_to_S typeExpr (cleanup t) of
  [] -> Nothing
  xs -> Just (last xs)

parseTypeExprFail :: String -> TypeExpr
-- ^ Same as `parseTypeExpr`, but throws error upon
-- failing to parse input completely.
parseTypeExprFail t = case parseTypeExpr t of
  Nothing -> error "Failed to parse expression!"
  Just (l, "") -> l
  Just (_, r) -> error ("Expression wasn't fully parsed! Remainder: " ++ r)

checkType :: LambdaExpr -> TypeExpr -> Bool
-- ^ Check that the given STLC expression has the given type
checkType = typeCheck emptyContext

inferType :: LambdaExpr -> Maybe TypeExpr
-- ^ Try to infer the type of the given STLC expression
inferType = typeInfer emptyContext

checkTypeIsom :: LambdaExpr -> TypeExpr -> Bool
-- ^ Check that the given STLC expression has the given type modulo isomorphism
checkTypeIsom = typeCheckIsom' emptyContext

inferTypeIsom :: LambdaExpr -> Maybe TypeTerm
-- ^ Try to infer the type of the given STLC expression modulo isomorphism
inferTypeIsom = typeInferIsom' emptyContext