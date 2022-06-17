{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module TypeTerm
  ( TypeVar,
    TypeTerm,
    typeTerm,
  )
where

import ParsingCommon
import Text.ParserCombinators.ReadP

newtype TypeVar = TypeVar {name :: String}
  deriving (Show)

data TypeTerm
  = TypeVariable {var :: TypeVar}
  | FunctionType {from :: TypeTerm, to :: TypeTerm}
  deriving (Show)

typeTerm :: ReadP TypeTerm
typeTerm = typeVariable <++ functionType

typeVariable :: ReadP TypeTerm
typeVariable = perhaps bracketed $ TypeVariable . TypeVar <$> capitalized

functionType :: ReadP TypeTerm
functionType = bracketed $ do
  from <- typeTerm
  string "->"
  to <- typeTerm
  return (FunctionType from to)