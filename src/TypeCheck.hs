module TypeCheck
  ( typeInfer,
    typeCheck,
  )
where

import Data.Functor ((<&>))
import LambdaTerm
import TypeTerm
import TypingContext

typeCheck :: TypingContext -> LambdaExpr -> TypeExpr -> Bool
typeCheck g (Variable v) tt = case lookupVar g v of
  Nothing -> False -- If the variable is not defined in context, we fail
  Just t -> t == tt -- Otherwise, do the types match?
typeCheck g (Constant c) tt = typeOfConst c == tt
typeCheck g (Abstraction v (TypeTerm _ at) bd) (TypeFunction (TypeTerm _ fr) to) =
  at == fr -- Are the argument types equal?
    && typeCheck (pushVar g v at) bd to -- Does the body check against the to type?
typeCheck g (Application fn ag) tt = case typeInfer g fn of
  Just (TypeFunction (TypeTerm _ fr) to) -- If we infer a function type
    | to == tt -- then does the result type match?
        && typeCheck g ag fr -> -- And does the arg check against from?
      True
  _ -> False -- If we infer nothing, or the wrong type, we fail
typeCheck g (Conditional co th el) tt =
  typeCheck g co (TypeConstant BooleanType) -- Is the condition boolean?
    && typeCheck g th tt -- And do the two branches
    && typeCheck g el tt -- both have the result type?
typeCheck _ _ _ = False

typeInfer :: TypingContext -> LambdaExpr -> Maybe TypeExpr
typeInfer g (Variable v) = lookupVar g v
typeInfer g (Constant c) = Just (typeOfConst c)
typeInfer g (Abstraction v t@(TypeTerm _ at) bd) = do
  t' <- typeInfer (pushVar g v at) bd -- Infer the type of the body, given v is in context
  return (TypeFunction t t')
typeInfer g (Application fn ag) = case typeInfer g fn of
  Just (TypeFunction (TypeTerm _ fr) to) -- If we infer a function type
    | typeCheck g ag fr -> -- does the argument type match the from?
      Just to
  _ -> Nothing -- If we infer nothing or the type does not match, we fail
typeInfer g (Conditional co th el) =
  if typeCheck g co (TypeConstant BooleanType) -- Is the condition boolean?
    then case typeInfer g th of -- Try infering the type of then
      Just t | typeCheck g el t -> Just t -- Does the else check against it?
      _ -> case typeInfer g el of -- Otherwise, try infering the type of else
        Just t | typeCheck g th t -> Just t -- Does the then check against it?
        _ -> Nothing
    else Nothing
