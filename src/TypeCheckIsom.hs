{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}

module TypeCheckIsom
  ( typeCheckIsom,
    typeInferIsom,
    typeCheckIsom',
    typeInferIsom',
  )
where

import Data.Maybe (fromMaybe)
import LambdaTerm
import TypeTerm
import TypingCommon

-- | Check that Left could be used in a place expecting Right.
--   Return resulting typing context and specialization.
subTypePoly :: TypeExpr -> TypeExpr -> Result (TypingContext TypeExpr)
subTypePoly = _stp emptyContext

subTypePoly' :: TypeTerm -> TypeTerm -> Result (TypingContext TypeExpr)
subTypePoly' = _stp' emptyContext

_stp :: TypingContext TypeExpr -> TypeExpr -> TypeExpr -> Result (TypingContext TypeExpr)
_stp d l@(TypeConstant bt) r@(TypeConstant bt') =
  if bt == bt' -- Base types have to be equal to be subtypes
    then success d
    else failure ("The type constants `" ++ show l ++ "` and `" ++ show r ++ "` are not equal.")
_stp d l@(TypeVariable vl) (TypeVariable v) = case lookupVar d v of
  Just te'@(TypeVariable v') ->
    if vl == v' -- If we find a type variable in the context, it has to be equal to the type variable
      then success d
      else failure ("Type variable `" ++ v ++ "` has been set to the type variable  `" ++ v' ++ "` which is not equal to the type variable `" ++ vl ++ "`")
  Just te' -> failure ("Type variable `" ++ v ++ "` has been set to `" ++ show te' ++ "` which is not type variable `" ++ vl ++ "`.")
  Nothing -> success (pushVar d v l) -- otherwise, we set the type variable to our left type variable
_stp d l (TypeVariable v) = case lookupVar d v of
  Just te' -> _stp d l te' /// show te' -- If the type variable has already been specialized, check for subtype
  Nothing -> success (pushVar d v l) -- otherwise, specialize the type variable
_stp d (TypeFunction fr to) (TypeFunction fr' to') = do
  d <- _stp' d fr fr' -- Froms have to be subtypes
  d <- _stp d to to' -- And tos have to to be subtypes, in the new context developed by the froms
  success d
_stp _ l r = failure ("The type expression `" ++ show l ++ "` can not be used in a place expecting `" ++ show r ++ "`.")

_stp' :: TypingContext TypeExpr -> TypeTerm -> TypeTerm -> Result (TypingContext TypeExpr)
_stp' d (TypeTerm ttg te) (TypeTerm ttg' te') =
  if ttg' <<= ttg -- The left tag has to be smaller than the right tag, not the otehr way round
    then _stp d te te'
    else failure ("The type tag `" ++ show ttg ++ "` is bigger than the type tag `" ++ show ttg' ++ "`.")

-- | Go through the type expression and substitute all type variables from the context
substTypeVars :: TypingContext TypeExpr -> TypeExpr -> TypeExpr
substTypeVars d te@(TypeVariable v) = fromMaybe te (lookupVar d v)
substTypeVars d te@(TypeFunction (TypeTerm ttg frte) to) = TypeFunction (TypeTerm ttg (substTypeVars d frte)) (substTypeVars d to)
substTypeVars d te = te

-- | Try to find an argument in Left that fits the type term Right.
--   Return reduced and specialized Left
applyArg :: TypeExpr -> TypeTerm -> Result TypeExpr
applyArg fn ag = do
  (d, te) <- aa fn ag
  success (substTypeVars d te)
  where
    aa :: TypeExpr -> TypeTerm -> Result (TypingContext TypeExpr, TypeExpr)
    aa (TypeFunction fr@(TypeTerm ttg te) to) ag@(TypeTerm ttg' te') =
      if ttg' <<= ttg -- Type tag of the argument has to be less than the type tag of the variable
        then case subTypePoly te' te of -- Check that the type itself fits
          Right d -> success (d, to) -- Return the remaining function and the specialized argument
          f@(Left _) ->
            descend |++ failure ("The type tag of `" ++ show fr ++ "` suits the type tag of `" ++ show ag ++ "`, but their types do not.") |++ f
        else descend |++ failure ("The type tag of the variable `" ++ show fr ++ "` is smaller than the type tag of the argument `" ++ show ag ++ "`.")
      where
        descend = do
          (d, te) <- aa to ag
          success (d, TypeFunction fr te)
    aa fn ag = failure ("Could not find an argument that fits `" ++ show ag ++ "`")

-- | Could Left be used in a place expecting Right?
--   Returns the specialized type expression.
(<:) :: TypeExpr -> TypeExpr -> Result TypeExpr
(<:) te te' = do
  d <- subTypePoly te te'
  success (substTypeVars d te')

-- | Check whether the lambda expression has the type in the context
typeCheckIsom :: TypingContext TypeExpr -> LambdaExpr -> TypeExpr -> Result TypeExpr
typeCheckIsom g (Variable v) te = case lookupVar g v of -- Look for variable type in emptyContext
  Just te' -> te' <: te -- Do the types fit?
  Nothing -> failure ("Could not find variable `" ++ v ++ "` in the context `" ++ show g ++ "`.")
typeCheckIsom g (Constant c) te = typeOfConst c <: te
typeCheckIsom g le@(Abstraction _ _ _) te@(TypeFunction _ _) = typeInferIsom g le >>= (<: te)
typeCheckIsom g (Application fn ag) te = do
  fnt <- typeInferIsom g fn -- Infer type of function
  agt <- typeInferIsom' g ag -- Infer type of argument
  rx <- applyArg fnt agt -- Apply & reduce
  rx <: te -- If it succeds, make sure the resulting types fit
typeCheckIsom _ lt tt = failure ("The lambda term `" ++ show lt ++ "` does not fit the type term `" ++ show tt ++ "`.")

-- | Check whether the lambda term has the type in the context
typeCheckIsom' :: TypingContext TypeExpr -> LambdaTerm -> TypeTerm -> Result TypeTerm
typeCheckIsom' g (LambdaTerm ltg le) (TypeTerm ttg te) = TypeTerm (ltg ||= ttg) <$> typeCheckIsom g le te

-- | Infer the type of the lambda expression in the context
typeInferIsom :: TypingContext TypeExpr -> LambdaExpr -> Result TypeExpr
typeInferIsom g (Variable v) = case lookupVar g v of
  Just lute -> success lute
  Nothing -> failure ("Could not find variable `" ++ v ++ "` in the context `" ++ show g ++ "`.")
typeInferIsom g (Constant c) = success (typeOfConst c)
typeInferIsom g (Abstraction v vte bd) = do
  te <- typeInferIsom (pushVar g v vte) bd -- Infer type of body, given argument
  success (TypeFunction (TypeTerm (Just v) vte) te) -- An argument variable's name becomes it's type's tag
typeInferIsom g (Application fn ag) = do
  fnt <- typeInferIsom g fn -- Infer type of function
  agt <- typeInferIsom' g ag
  applyArg fnt agt -- Try to apply the function to the argument
typeInferIsom g (Let lv le lb) = do
  te <- typeInferIsom g le
  te' <- typeInferIsom (pushVar g lv te) lb
  success te'

-- | Infer the type of the lambda term in the context
typeInferIsom' :: TypingContext TypeExpr -> LambdaTerm -> Result TypeTerm
typeInferIsom' g (LambdaTerm ltg le) = TypeTerm ltg <$> typeInferIsom g le