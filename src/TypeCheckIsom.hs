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
import Data.Maybe (fromMaybe, isJust, isNothing)
import Debug.Trace (traceShow)
import LambdaTerm
import TypeTerm
import TypingCommon

-- | Could the first expression be used in a place expecting the second expression?
--   Returns the resulting specialization of type variables in the second expression.
subTypePoly :: TypeExpr -> TypeExpr -> Result (TypingContext TypeExpr)
subTypePoly = _subTypePoly emptyContext

_subTypePoly :: TypingContext TypeExpr -> TypeExpr -> TypeExpr -> Result (TypingContext TypeExpr)
_subTypePoly d (TypeConstant bt) (TypeConstant bt') =
  if bt == bt' -- Base types can only be subtypes of themselves
    then success d
    else failure ("The base types `" ++ show bt ++ "` and `" ++ show bt' ++ "` are incompatible.")
_subTypePoly d (TypeFunction fr to) (TypeFunction fr' to') =
  _subTypePoly' d fr fr' -- the from types has to be subtypes
    >>= \d -> _subTypePoly d to to' -- and the to types have to be be subtypes
_subTypePoly d te (TypeVariable tv) = case lookupVar d tv of
  Just te' -> _subTypePoly d te te' -- If the type variable has already been specialized, check for subtype
  Nothing -> success (pushVar d tv te) -- otherwise, specialize the type variable
_subTypePoly d (TypeVariable tv) te = failure ("The type variable `" ++ tv ++ "` is not a general subtype of the type expression `" ++ show te ++ "`.")
_subTypePoly _ te te' = failure ("The type expression `" ++ show te ++ "` is not a subtype of the type expression `" ++ show te' ++ "`.")

-- | Could the first term be used in a place expecting the second term?
--   Returns the resulting context of type variables.
subTypePoly' :: TypeTerm -> TypeTerm -> Result (TypingContext TypeExpr)
subTypePoly' = _subTypePoly' emptyContext

_subTypePoly' :: TypingContext TypeExpr -> TypeTerm -> TypeTerm -> Result (TypingContext TypeExpr)
_subTypePoly' d (TypeTerm tg te) (TypeTerm tg' te') =
  if tg <<= tg'
    then _subTypePoly d te te'
    else failure ("The tag `" ++ show tg ++ "` is not a subtag of `" ++ show tg' ++ "`.")

-- | Could left be used in a place expecting right?
--   Returns the specialized type expression.
(<:) :: TypeExpr -> TypeExpr -> Result TypeExpr
(<:) te te' = do
  d <- subTypePoly te te'
  success (substTypeVars d te')

-- | Could left be used in a place expecting right?
--   Returns the specialized type term.
(<<:) :: TypeTerm -> TypeTerm -> Result TypeTerm
(<<:) tt tt'@(TypeTerm tg te) =
  do
    d <- subTypePoly' tt tt'
    success (TypeTerm tg (substTypeVars d te))

-- | Go through the type expression and replace all occurences of type variables
--   with their respective specialized type, if present in the given context.
substTypeVars :: TypingContext TypeExpr -> TypeExpr -> TypeExpr
substTypeVars d te@(TypeVariable tv) = fromMaybe te (lookupVar d tv)
substTypeVars d te@(TypeConstant _) = te
substTypeVars d (TypeFunction (TypeTerm tt te) te') = TypeFunction (TypeTerm tt (substTypeVars d te)) (substTypeVars d te')

(///) :: Show a => b -> a -> b
(///) x y = if debugEnabled then traceShow y x else x
  where
    debugEnabled = True

typeCheckIsom :: TypingContext TypeExpr -> LambdaTerm -> TypeTerm -> Result TypeTerm
typeCheckIsom g lt@(LambdaTerm ltg _) tt@(TypeTerm ttg _) = do
  te <- tci g lt tt
  success (TypeTerm (ltg ||= ttg) te)
  where
    tci :: TypingContext TypeExpr -> LambdaTerm -> TypeTerm -> Result TypeExpr
    tci g lt@(LambdaTerm ltg (Variable v)) tt@(TypeTerm tg te) = case lookupVar g v of -- Look for variable type in emptyContext
      Just te' -> te' <: te -- Do the types fit?
      Nothing -> failure ("Could not find variable `" ++ v ++ "` in the context `" ++ show g ++ "`.")
    tci g lt@(LambdaTerm _ (Constant c)) (TypeTerm _ te) = typeOfConst c <: te
    tci g lt@(LambdaTerm _ le@(Abstraction _ _ _)) (TypeTerm _ (TypeFunction fr to)) = do
      (le', (v', vte')) <- findArgVar le fr -- Find argument variable in abstraction that fits the function's from
      (TypeTerm _ te) <- typeCheckIsom' (pushVar g v' vte') le' to -- Does the reduced lambda expression fit the to?
      success te
    tci g lt@(LambdaTerm _ (Application fn ag)) (TypeTerm _ te) = do
      (TypeTerm ftg ft) <- typeInferIsom' g fn -- Infer type of function
      ag <- applyArg g ft ag -- And try to apply argument to it
      ag <: te -- If it succeds, make sure the resulting types fit
    tci _ lt tt = failure ("The lambda term `" ++ show lt ++ "` does not fit the type term `" ++ show tt ++ "`.")

typeCheckIsom' :: TypingContext TypeExpr -> LambdaExpr -> TypeExpr -> Result TypeTerm
typeCheckIsom' g le te = typeCheckIsom g (LambdaTerm Nothing le) (TypeTerm Nothing te)

typeInferIsom :: TypingContext TypeExpr -> LambdaTerm -> Result TypeTerm
typeInferIsom g (LambdaTerm ltg le) = TypeTerm ltg <$> tii g le
  where
    tii :: TypingContext TypeExpr -> LambdaExpr -> Result TypeExpr
    tii g (Variable v) = case lookupVar g v of
      Just lute -> success lute
      Nothing -> failure ("Could not find variable `" ++ v ++ "` in the context `" ++ show g ++ "`.")
    tii g (Constant c) = success (typeOfConst c)
    tii g (Abstraction v vte bd) = do
      (TypeTerm _ te) <- typeInferIsom' (pushVar g v vte) bd -- Infer type of body, given argument
      success (TypeFunction vtt te)
      where
        vtt = TypeTerm (Just v) vte -- An argument variable's name becomes it's type's tag
    tii g (Application fn ag) = do
      (TypeTerm _ te) <- typeInferIsom' g fn -- Infer type of function
      applyArg g te ag -- Try to apply the function to the argument
    tii g (Let lv le lb) = do
      (TypeTerm _ te) <- typeInferIsom' g le
      (TypeTerm _ te') <- typeInferIsom' (pushVar g lv te) lb
      success te'

typeInferIsom' :: TypingContext TypeExpr -> LambdaExpr -> Result TypeTerm
typeInferIsom' g le = typeInferIsom g (LambdaTerm Nothing le)

-- | Descend into a given function type and try to find
--   an argument that fits the given lambda term.
--   Return the reduced function expression if successful.
applyArg :: TypingContext TypeExpr -> TypeExpr -> LambdaTerm -> Result TypeExpr
applyArg g te ag = do
  case aa g te ag of
    Right (r, d) -> success (substTypeVars d r)
    Left s -> failure s
  where
    -- Left s -> case aa g te (LambdaTerm Nothing (lambdaExpr' ag)) of
    --   Right (r, d) -> success (substTypeVars d r)
    --   Left s' -> failure (s ++ "\n" ++ s')

    aa :: TypingContext TypeExpr -> TypeExpr -> LambdaTerm -> Result (TypeExpr, TypingContext TypeExpr)
    aa g (TypeFunction fr@(TypeTerm frtg frte) to) ag = case do
      (TypeTerm ttg te) <- typeInferIsom g ag -- Find type of argument
      case ttg of
        Just _ ->
          -- If the argument has a tag
          if ttg <<= frtg -- make sure that it fits the tag of the from
            then subTypePoly te frte -- and check that the expression fits
            else failure ("The tag of the argument `" ++ show ag ++ "` does not fit the tag of the from `" ++ show fr ++ "`")
        Nothing -> subTypePoly te frte of -- otherwise, just check that the expression fits
      Right d -> success (to, d) /// ("Found " ++ show fr ++ " for " ++ show ag)
      Left s -> case aa g to ag of
        Right (r, d) -> success (TypeFunction fr r, d)
        Left s' -> failure (s ++ "\n" ++ s')
    aa _ te lt = failure ("Could not find an argument that fits the lambda term `" ++ show lt ++ "`.")

-- | Descend into a given abstraction and find an argument variable
--   that fits the given TypeTerm. If successful,
--   returns the reduced abstraction and the cut out variable.
findArgVar :: LambdaExpr -> TypeTerm -> Result (LambdaExpr, (String, TypeExpr))
findArgVar (Abstraction v vte bd) fr = case subTypePoly' fr (TypeTerm (Just v) vte) of
  Right d -> success (bd, (v, substTypeVars d vte))
  Left s -> case findArgVar bd fr of
    Right (bd', va') -> success (Abstraction v vte bd', va')
    Left s' -> failure (s ++ s')
findArgVar le tt = failure ("Could not find an argument variable that fits the type term `" ++ show tt ++ "`.")

-- | Flatten a given function type into a list of it's froms
--   and it's to.
flattenTypeFunction :: TypeExpr -> ([TypeTerm], TypeExpr)
flattenTypeFunction te = (init ftd, typeExpr' (last ftd))
  where
    ftd = reverse (ft te)
    ft :: TypeExpr -> [TypeTerm]
    ft (TypeFunction fr to) = fr : ft to
    ft te = [TypeTerm Nothing te]

-- | Flatten a given abstraction expression into a list of it's argument variables
--   and it's body.
flattenAbstraction :: LambdaExpr -> ([(String, TypeExpr)], LambdaExpr)
flattenAbstraction (Abstraction v vte bd) = ((v, vte) : vas, bd)
  where
    (vas, bd) = flattenAbstraction bd
flattenAbstraction le = ([], le)

-- | Flatten a given application expression into the function
--    and a list of all it's arguments.
flattenApplication :: LambdaExpr -> (LambdaExpr, [LambdaTerm])
flattenApplication le = (lambdaExpr' (head fld), tail fld)
  where
    fld = reverse (fl le)
    fl :: LambdaExpr -> [LambdaTerm]
    fl (Application fn ag) = ag : fl fn
    fl le = [LambdaTerm Nothing le]