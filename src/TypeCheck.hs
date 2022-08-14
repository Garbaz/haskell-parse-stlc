module TypeCheck
  ( typeInfer,
    typeCheck,
  )
where

import LambdaTerm
import TypeTerm
import TypingCommon

typeCheck :: TypingContext TypeExpr -> LambdaExpr -> TypeExpr -> Bool
typeCheck g (Variable v) te = lookupVar g v >>=? \t -> t == te
typeCheck g (Constant c) te = typeOfConst c == te
typeCheck g (Abstraction v vte bd) (TypeFunction (TypeTerm _ fr) to) =
  vte == fr -- Are the argument types equal?
    && typeCheck (pushVar g v vte) bd to -- Does the body check against the to type?
typeCheck g (Application fn (LambdaTerm _ ag)) te = case typeInfer g fn of
  Just (TypeFunction (TypeTerm _ fr) to) -- If we infer a function type
    | to == te -- then does the result type match?
        && typeCheck g ag fr -> -- And does the arg check against from?
      True
  _ -> False -- If we infer nothing, or the wrong type, we fail
  -- typeCheck g (Conditional co th el) te =
  --   typeCheck g co (TypeConstant BooleanType) -- Is the condition boolean?
  --     && typeCheck g th te -- And do the two branches...
  --     && typeCheck g el te -- ...both have the result type?
typeCheck _ _ _ = False

typeInfer :: TypingContext TypeExpr -> LambdaExpr -> Maybe TypeExpr
typeInfer g (Variable v) = lookupVar g v
typeInfer g (Constant c) = Just (typeOfConst c)
typeInfer g (Abstraction v vte bd) = do
  t <- typeInfer (pushVar g v vte) bd -- Infer the type of the body, given v is in context
  Just (TypeFunction (TypeTerm (Just v) vte) t)
typeInfer g (Application fn (LambdaTerm _ ag)) = do
  TypeFunction (TypeTerm _ fr) to <- typeInfer g fn -- Infer a function type
  typeCheck g ag fr ?>> Just to -- Does the argument check against from?
  -- typeInfer g (Conditional co th el) =
  --   typeCheck g co (TypeConstant BooleanType) -- Is the condition boolean?
  --     ?>> case typeInfer g th of -- Try infering the type of then-case
  --       Just t | typeCheck g el t -> Just t -- Does else-case check against it?
  --       _ -> case typeInfer g el of -- Otherwise, try infering the type of else-case
  --         Just t | typeCheck g th t -> Just t -- Does then-case check against it?
  --         _ -> Nothing
