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
--   Returns the resulting context of type variables.
subTypePoly :: TypeExpr -> TypeExpr -> Error (TypingContext TypeExpr)
subTypePoly = _subTypePoly emptyContext

_subTypePoly :: TypingContext TypeExpr -> TypeExpr -> TypeExpr -> Error (TypingContext TypeExpr)
_subTypePoly d (TypeConstant bt) (TypeConstant bt') =
  if bt == bt'
    then success d
    else failure ("The type `" ++ show bt ++ "` is not a subtype of `" ++ show bt' ++ "`.")
_subTypePoly d (TypeFunction tt te) (TypeFunction tt' te') = _subTypePoly' d tt tt' >>= \d -> _subTypePoly d te te'
_subTypePoly d te (TypeVariable tv) = case lookupVar d tv of
  Just te' -> _subTypePoly d te te'
  Nothing -> success (pushVar d tv te)
_subTypePoly d (TypeVariable tv) te = success (pushVar d tv te)
_subTypePoly _ _ _ = undefined

-- | Could the first term be used in a place expecting the second term?
--   Returns the resulting context of type variables.
subTypePoly' :: TypeTerm -> TypeTerm -> Error (TypingContext TypeExpr)
subTypePoly' = _subTypePoly' emptyContext

_subTypePoly' :: TypingContext TypeExpr -> TypeTerm -> TypeTerm -> Error (TypingContext TypeExpr)
_subTypePoly' d (TypeTerm tg te) (TypeTerm tg' te') =
  if tg <<= tg'
    then _subTypePoly d te te'
    else failure ("The tag `" ++ show tg ++ "` is not a subtag of `" ++ show tg' ++ "`.")

-- | Could left be used in a place expecting right?
--   Returns the specialized type expression.
(<:) :: TypeExpr -> TypeExpr -> Error TypeExpr
-- (<:) (TypeConstant bt) (TypeConstant bt') = bt == bt'
-- (<:) (TypeFunction tt te) (TypeFunction tt' te') = (tt <<: tt') && (te <: te')
-- (<:) _ _ = False
-- (<:) te te' = isJust (subTypePoly emptyContext te te')
(<:) te te' = do
  d <- subTypePoly te te'
  success (substTypeVars d te')

-- | Could left be used in a place expecting right?
--   Returns the specialized type term.
(<<:) :: TypeTerm -> TypeTerm -> Error TypeTerm
-- (<<:) (TypeTerm tg te) (TypeTerm tg' te') = tg <<= tg' && te <: te'
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

typeCheckIsom :: TypingContext TypeExpr -> LambdaTerm -> TypeTerm -> Error TypeTerm
typeCheckIsom g lt@(LambdaTerm ltg _) tt@(TypeTerm ttg _) = do
  te <- tci g lt tt
  success (TypeTerm (ltg ||= ttg) te)
  where
    tci :: TypingContext TypeExpr -> LambdaTerm -> TypeTerm -> Error TypeExpr
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
      ag <: te -- If it succeds, make sure the resulting type fits
    tci _ lt tt = failure ("The lambda term `" ++ show lt ++ "` does not fit the type term `" ++ show tt ++ "`.")

typeCheckIsom' :: TypingContext TypeExpr -> LambdaExpr -> TypeExpr -> Error TypeTerm
typeCheckIsom' g le te = typeCheckIsom g (LambdaTerm Nothing le) (TypeTerm Nothing te)

typeInferIsom :: TypingContext TypeExpr -> LambdaTerm -> Error TypeTerm
typeInferIsom g (LambdaTerm ltg (Variable v)) = case lookupVar g v of
  Just lute -> success (TypeTerm ltg lute)
  Nothing -> failure ("Could not find variable `" ++ v ++ "` in the context `" ++ show g ++ "`.")
typeInferIsom g (LambdaTerm ltg (Constant c)) = success (TypeTerm ltg (typeOfConst c))
typeInferIsom g (LambdaTerm ltg (Abstraction v vte bd)) = do
  (TypeTerm _ te) <- typeInferIsom' (pushVar g v vte) bd -- Infer type of body, given argument
  success (TypeTerm ltg (TypeFunction vtt te))
  where
    vtt = TypeTerm (Just v) vte -- An argument variable's name becomes it's type's tag
typeInferIsom g (LambdaTerm ltg (Application fn ag)) = do
  (TypeTerm _ te) <- typeInferIsom' g fn -- Infer type of function
  to <- applyArg g te ag -- Try to apply the function to the argument
  success (TypeTerm ltg to)

-- typeInferIsom g (LambdaTerm ltg (Conditional co th el)) =
--   isJust (typeCheckIsom' g co (TypeConstant BooleanType)) -- Is the condition boolean?
--     ?>> case typeInferIsom' g th of -- Try infering the type of then-case
--       Just t | typeCheckIsom' g el (typeExpr' t) -> Just t -- Does else-case check against it?
--       _ -> case typeInferIsom' g el of -- Otherwise, try infering the type of else-case
--         Just t | typeCheckIsom' g th (typeExpr' t) -> Just t -- Does then-case check against it?
--         _ -> Nothing

typeInferIsom' :: TypingContext TypeExpr -> LambdaExpr -> Error TypeTerm
typeInferIsom' g le = typeInferIsom g (LambdaTerm Nothing le)

-- | Descend into a given function type and try to find
--   an argument that fits the given lambda term.
--   Return the reduced function expression if successful.
applyArg :: TypingContext TypeExpr -> TypeExpr -> LambdaTerm -> Error TypeExpr
applyArg g (TypeFunction fr to) ag = case typeInferIsom g ag of
  Right tt -> do
    d <- subTypePoly' tt fr
    success (substTypeVars d to)
  Left _ -> TypeFunction fr <$> applyArg g to ag
--   (TypeTerm tt (TypeVariable tv)) -> substTypeVars (pushVar emptyContext tv a)
--   _ -> Just to -- Does the argument type fit the from type?

applyArg _ te lt =
  failure
    ( "Could not find an argument that fits the lambda term `"
        ++ show lt
        ++ "` in the type expression `"
        ++ show te
        ++ "`."
    )

-- | Descend into a given abstraction and find an argument variable
--   that fits the given TypeTerm. If successful,
--   returns the reduced abstraction and the cut out variable.
findArgVar :: LambdaExpr -> TypeTerm -> Error (LambdaExpr, (String, TypeExpr))
findArgVar (Abstraction v vte bd) fr = case subTypePoly' fr (TypeTerm (Just v) vte) of
  Right d -> success (bd, (v, substTypeVars d vte))
  Left _ -> do
    (bd', va') <- findArgVar bd fr
    success (Abstraction v vte bd', va')
findArgVar le tt =
  failure
    ( "Could not find an argument variable that fits the type term `" ++ show tt ++ "` in the lambda expression `" ++ show le
        ++ "`."
    )

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