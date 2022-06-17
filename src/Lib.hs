module Lib
  ( module LambdaTerm,
    module TypeTerm,
    parseSTLC,
  )
where

import LambdaTerm
import Text.ParserCombinators.ReadP
import TypeTerm

parseSTLC :: String -> (LambdaTerm, String)
-- ^ Parses a given Simply Typed Lambda Calculus term
--   as specified in the 'README.md' to the AST.
--   Whitespace is entirely ignored.
parseSTLC = last . readP_to_S lambdaTerm . filter (/= ' ')