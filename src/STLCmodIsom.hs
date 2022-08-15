module STLCmodIsom
  ( parseSTLC,
    parseSTLCFail,
    parseTypeExpr,
    parseTypeExprFail,
    -- checkType,
    -- inferType,
    checkTypeIsom,
    inferTypeIsom,
    -- repl,
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

import Control.Exception (try)
import LambdaTerm
import System.Console.Repl
import Text.ParserCombinators.ReadP (readP_to_S)
import TypeCheck
import TypeCheckIsom
import TypeTerm
import TypingCommon (Result, emptyContext)

cleanup :: String -> String
cleanup = filter (`notElem` [' ', '\n', '\t'])

-- | Parses a given Simply Typed Lambda Calculus term
--   as specified in the 'README.md' to the AST.
--   Whitespace is entirely ignored.
parseSTLC :: String -> Maybe (LambdaExpr, String)
parseSTLC e = case readP_to_S lambdaExpr (cleanup e) of
  [] -> Nothing
  xs -> Just (last xs)

-- | Same as `parseSTLC`, but throws error upon
--   failing to parse input completely.
parseSTLCFail :: String -> LambdaExpr
parseSTLCFail e = case parseSTLC e of
  Nothing -> error "Failed to parse expression!"
  Just (l, "") -> l
  Just (_, r) -> error ("Expression wasn't fully parsed! Remainder: `" ++ r ++ "`")

-- | Parses a given TypeExpr term
--   as specified in the 'README.md' to the Type-AST.
--   Whitespace is entirely ignored.
parseTypeExpr :: String -> Maybe (TypeExpr, String)
parseTypeExpr t = case readP_to_S typeExpr (cleanup t) of
  [] -> Nothing
  xs -> Just (last xs)

-- | Same as `parseTypeExpr`, but throws error upon
--   failing to parse input completely.
parseTypeExprFail :: String -> TypeExpr
parseTypeExprFail t = case parseTypeExpr t of
  Nothing -> error "Failed to parse expression!"
  Just (l, "") -> l
  Just (_, r) -> error ("Expression wasn't fully parsed! Remainder: `" ++ r ++ "`")

-- | Check that the given STLC expression has the given type
checkType :: LambdaExpr -> TypeExpr -> Bool
checkType = typeCheck emptyContext

-- | Try to infer the type of the given STLC expression
inferType :: LambdaExpr -> Maybe TypeExpr
inferType = typeInfer emptyContext

-- | Check that the given STLC expression has the given type modulo isomorphism
checkTypeIsom :: LambdaExpr -> TypeExpr -> Result TypeTerm
checkTypeIsom = typeCheckIsom' emptyContext

-- | Try to infer the type of the given STLC expression modulo isomorphism
inferTypeIsom :: LambdaExpr -> Result TypeTerm
inferTypeIsom = typeInferIsom' emptyContext

replInferIsom :: IO ()
replInferIsom =
  repl
    "|- "
    ( \l -> case parseSTLC l of
        Just (e, "") -> print (inferTypeIsom e)
        _ -> print "Failed to parse input."
    )

-- repl :: (Read a, Show b) => (a -> b) -> IO ()
-- repl f = do
--   i <- readLn
--   print (f i)
--   repl f
