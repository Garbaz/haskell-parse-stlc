module ParseSTLC
  ( module LambdaTerm,
    module TypeTerm,
    parseSTLC,
    parseTypeTerm,
    checkType,
  )
where

import LambdaTerm
import Text.ParserCombinators.ReadP
import TypeCheck
import TypeTerm

cleanup :: String -> String
cleanup = filter (/= ' ')

parseSTLC :: String -> Maybe (LambdaExpr, String)
-- ^ Parses a given Simply Typed Lambda Calculus term
--   as specified in the 'README.md' to the AST.
--   Whitespace is entirely ignored.
parseSTLC e = case readP_to_S lambdaExpr (cleanup e) of
  [] -> Nothing
  xs -> Just (last xs)

parseSTLCfail :: String -> LambdaExpr
parseSTLCfail e = case readP_to_S lambdaExpr (cleanup e) of
  [] -> error "Failed to parse expression!"
  xs -> case last xs of
    (l, r) -> if r == "" then l else error ("Expression wasn't fully parsed! Remainder: " ++ r)

parseTypeTerm :: String -> Maybe (TypeExpr, String)
parseTypeTerm t = case readP_to_S typeExpr (cleanup t) of
  [] -> Nothing
  xs -> Just (last xs)

checkType :: LambdaExpr -> TypeExpr -> Bool
checkType = typeCheck emptyContext

inferType :: LambdaExpr -> Maybe TypeExpr
inferType = typeInfer emptyContext