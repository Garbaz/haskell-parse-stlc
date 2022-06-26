module ParseSTLC
  ( module LambdaTerm,
    module TypeTerm,
    parseSTLC,
  )
where

import LambdaTerm
import Text.ParserCombinators.ReadP
import TypeCheck
import TypeTerm

parseSTLC :: String -> Maybe (LambdaTerm, String)
-- ^ Parses a given Simply Typed Lambda Calculus term
--   as specified in the 'README.md' to the AST.
--   Whitespace is entirely ignored.
parseSTLC e = case readP_to_S lambdaTerm (filter (/= ' ') e) of
  [] -> Nothing
  xs -> Just (last xs)