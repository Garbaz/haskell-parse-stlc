{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}
module TypeCheckIsom
  ( typeCheckIsom,
    typeInferIsom,
    typeCheckIsom',
    typeInferIsom',
  )
where

import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import Debug.Trace
import LambdaTerm
import TypeCheck
import TypeTerm
import TypingCommon

(///) :: Show a => b -> a -> b
(///) x y = if debugEnabled then traceShow y x else x
  where
    debugEnabled = True

typeCheckIsom :: TypingContext TypeExpr -> LambdaTerm -> TypeTerm -> Bool
typeCheckIsom g lt@(LambdaTerm ltg _) tt@(TypeTerm ttg _) = ltg <<= ttg && tci g lt tt
  where
    tci :: TypingContext TypeExpr -> LambdaTerm -> TypeTerm -> Bool
    tci g lt@(LambdaTerm ltg (Variable v)) (TypeTerm _ te) =
      lookupVar g v -- Look for variable type in context
        >>=? (<: te) -- Do the types fit?
    tci g lt@(LambdaTerm _ (Constant c)) (TypeTerm _ te) = typeOfConst c <: te
    tci g lt@(LambdaTerm _ le@(Abstraction _ _ _)) (TypeTerm _ (TypeFunction fr to)) =
      findArgVar le fr -- Find argument variable in abstraction that fits the function's from
        >>=? \(le', (v', vte')) ->
          typeCheckIsom' (pushVar g v' vte') le' to -- Does the reduced lambda expression fit the to?
    tci g lt@(LambdaTerm _ (Application fn ag)) (TypeTerm _ te) =
      ( typeInferIsom' g fn -- Infer type of function
          >>= \(TypeTerm ftg ft) -> applyArg g ft ag -- And try to apply argument to it
      )
        >>=? (<: te) -- If it succeds, make sure the resulting type fits
    tci _ _ _ = False

typeCheckIsom' :: TypingContext TypeExpr -> LambdaExpr -> TypeExpr -> Bool
typeCheckIsom' g le te = typeCheckIsom g (LambdaTerm Nothing le) (TypeTerm Nothing te)

typeInferIsom :: TypingContext TypeExpr -> LambdaTerm -> Maybe TypeTerm
typeInferIsom g (LambdaTerm ltg (Variable v)) = lookupVar g v >>= \lute -> Just (TypeTerm ltg lute)
typeInferIsom g (LambdaTerm ltg (Constant c)) = Just (TypeTerm ltg (typeOfConst c))
typeInferIsom g (LambdaTerm ltg (Abstraction v vte bd)) = do
  (TypeTerm _ te) <- typeInferIsom' (pushVar g v vte) bd -- Infer type of body, given argument
  Just (TypeTerm ltg (TypeFunction vtt te))
  where
    vtt = TypeTerm (Just v) vte -- An argument variable's name becomes it's type's tag
typeInferIsom g (LambdaTerm ltg (Application fn ag)) = do
  (TypeTerm _ te) <- typeInferIsom' g fn -- Infer type of function
  to <- applyArg g te ag -- Try to apply the function to the argument
  Just (TypeTerm ltg to)
typeInferIsom g (LambdaTerm ltg (Conditional co th el)) =
  typeCheckIsom' g co (TypeConstant BooleanType) -- Is the condition boolean?
    ?>> case typeInferIsom' g th of -- Try infering the type of then-case
      Just t | typeCheckIsom' g el (typeExpr' t) -> Just t -- Does else-case check against it?
      _ -> case typeInferIsom' g el of -- Otherwise, try infering the type of else-case
        Just t | typeCheckIsom' g th (typeExpr' t) -> Just t -- Does then-case check against it?
        _ -> Nothing

typeInferIsom' :: TypingContext TypeExpr -> LambdaExpr -> Maybe TypeTerm
typeInferIsom' g le = typeInferIsom g (LambdaTerm Nothing le)

applyArg :: TypingContext TypeExpr -> TypeExpr -> LambdaTerm -> Maybe TypeExpr
-- ^ Descend into a given function type and try to find
--   an argument that fits the given lambda term.
--   Return the reduced function expression if successful.
applyArg g (TypeFunction fr to) ag
  | typeCheckIsom g ag fr = Just to -- Does the argument type fit the from type?
  | otherwise = TypeFunction fr <$> applyArg g to ag -- If not, search for fitting argument type further down
applyArg _ _ _ = Nothing

findArgVar :: LambdaExpr -> TypeTerm -> Maybe (LambdaExpr, (String, TypeExpr))
-- ^ Decent into a given abstraction and find an argument variable
--   that fits the given TypeTerm. If successful,
--   returns the reduced abstraction and the cut out variable.
findArgVar (Abstraction v vte bd) fr
  | fr <<: TypeTerm (Just v) vte = Just (bd, (v, vte)) -- Does the from type fit the argument variable type?
  | otherwise = do
    (bd', va') <- findArgVar bd fr -- If not, search for fitting argument variable further down.
    Just (Abstraction v vte bd', va')
findArgVar _ _ = Nothing

flattenTypeFunction :: TypeExpr -> ([TypeTerm], TypeExpr)
-- ^ Flatten a given function type into a list of it's froms
--   and it's to.
flattenTypeFunction te = (init ftd, typeExpr' (last ftd))
  where
    ftd = reverse (ft te)
    ft :: TypeExpr -> [TypeTerm]
    ft (TypeFunction fr to) = fr : ft to
    ft te = [TypeTerm Nothing te]

flattenAbstraction :: LambdaExpr -> ([(String, TypeExpr)], LambdaExpr)
-- ^ Flatten a given abstraction expression into a list of it's argument variables
--   and it's body.
flattenAbstraction (Abstraction v vte bd) = ((v, vte) : vas, bd)
  where
    (vas, bd) = flattenAbstraction bd
flattenAbstraction le = ([], le)

flattenApplication :: LambdaExpr -> (LambdaExpr, [LambdaTerm])
-- ^ Flatten a given application expression into the function
--   and a list of all it's arguments.
flattenApplication le = (lambdaExpr' (head fld), tail fld)
  where
    fld = reverse (fl le)
    fl :: LambdaExpr -> [LambdaTerm]
    fl (Application fn ag) = ag : fl fn
    fl le = [LambdaTerm Nothing le]