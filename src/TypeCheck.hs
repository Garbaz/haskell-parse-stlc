module TypeCheck
  ( typeInfer,
    typeCheck,
    emptyContext
  )
where

import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import LambdaTerm
import TypeTerm

type TypingContext = Map.Map String [TypeExpr]

emptyContext = Map.empty

lookupVar :: TypingContext -> String -> Maybe TypeExpr
lookupVar g v = head <$> Map.lookup v g

pushVar :: TypingContext -> String -> TypeExpr -> TypingContext
pushVar g v t = Map.insert v (t : fromMaybe [] (Map.lookup v g)) g

condMaybe :: Bool -> Maybe a -> Maybe a
condMaybe p x = if p then x else Nothing

(~=) :: TypeTerm -> TypeTerm -> Bool
(~=) (TypeTerm _ e) (TypeTerm _ e') = e == e'

typeCheck :: TypingContext -> LambdaExpr -> TypeExpr -> Bool
typeCheck g (Variable v) t = case lookupVar g v of
  Nothing -> False
  Just t' -> t' == t
typeCheck g (Constant c) t = typeOfConst c == t
typeCheck g (Abstraction v t@(TypeTerm _ t') b) (TypeFunction fr to) = t ~= fr && typeCheck (pushVar g v t') b to
typeCheck g (Application f (LambdaTerm _ a)) t = case typeInfer g f of
  Just (TypeFunction (TypeTerm _ fr) to) -> to == t && typeCheck g a fr
  _ -> False
typeCheck g (Conditional co th el) t =
  typeCheck g co (TypeConstant BooleanType)
    && typeCheck g th t
    && typeCheck g el t
typeCheck _ _ _ = False

typeInfer :: TypingContext -> LambdaExpr -> Maybe TypeExpr
typeInfer g (Variable v) = lookupVar g v
typeInfer g (Constant c) = Just (typeOfConst c)
typeInfer g (Abstraction v t@(TypeTerm _ t') b) = do
  t'' <- typeInfer (pushVar g v t') b
  return (TypeFunction t t'')
typeInfer g (Application f (LambdaTerm _ a)) = case typeInfer g f of
  Just (TypeFunction (TypeTerm _ fr) to) | typeCheck g a fr -> Just to
  _ -> Nothing
typeInfer g (Conditional co th el) =
  if typeCheck g co (TypeConstant BooleanType)
    then do
      t <- typeInfer g th
      t' <- typeInfer g el
      condMaybe (t == t') (Just t)
    else Nothing
