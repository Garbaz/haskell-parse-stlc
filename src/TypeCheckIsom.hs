module TypeCheckIsom
  ( typeCheckIsom,
    typeInferIsom,
    typeCheckIsom',
    typeInferIsom',
  )
where

import Data.Maybe (fromMaybe, isNothing)
import LambdaTerm
import TypeCheck
import TypeTerm
import TypingCommon

typeCheckIsom :: TypingContext TypeTerm -> LambdaTerm -> TypeTerm -> Bool
typeCheckIsom g lt@(LambdaTerm ltg _) tt@(TypeTerm ttg _) = ltg <<= ttg && tci g lt tt
  where
    tci :: TypingContext TypeTerm -> LambdaTerm -> TypeTerm -> Bool
    tci g lt@(LambdaTerm ltg (Variable v)) (TypeTerm ttg te) =
      lookupVar g v -- Look for variable type in context
        >>=? \(TypeTerm tg t) ->
          (ltg ||= tg) <<= ttg -- Do the tags fit (with local tag over context tag)?
            && t <: te -- Do the types fit?
    tci g lt@(LambdaTerm _ (Constant c)) (TypeTerm _ te) = typeOfConst c <: te
    tci g lt@(LambdaTerm _ (Abstraction v vte bd)) (TypeTerm _ te@(TypeFunction fr to)) =
      findNamedArg te vtt -- Look for fitting argument in function type
        >>=? \t ->
          typeCheckIsom' (pushVar g v vtt) bd t -- Does the body fit the reduced type?
      where
        vtt = TypeTerm (Just v) vte -- An argument variable's name becomes it's type's tag
    tci g lt@(LambdaTerm _ (Application fn ag)) (TypeTerm _ te) =
      ( typeInferIsom' g fn -- Infer type of function
          >>= \(TypeTerm ftg ft) -> applyNamedArg g ft ag -- And try to apply argument to it
      )
        >>=? (<: te) -- If it succeds, make sure the resulting type fits
    tci _ _ _ = False

typeCheckIsom' :: TypingContext TypeTerm -> LambdaExpr -> TypeExpr -> Bool
typeCheckIsom' g le te = typeCheckIsom g (LambdaTerm Nothing le) (TypeTerm Nothing te)

typeInferIsom :: TypingContext TypeTerm -> LambdaTerm -> Maybe TypeTerm
typeInferIsom g (LambdaTerm ltg (Variable v)) = do
  (TypeTerm lutg lute) <- lookupVar g v -- Look up variable in context
  Just (TypeTerm (ltg ||= lutg) lute) -- and overwrite tag, if given
typeInferIsom g (LambdaTerm ltg (Constant c)) = Just (TypeTerm ltg (typeOfConst c))
typeInferIsom g (LambdaTerm ltg (Abstraction v vte bd)) = do
  (TypeTerm _ t) <- typeInferIsom' (pushVar g v vtt) bd -- Infer type of body, given argument
  Just (TypeTerm ltg (TypeFunction vtt t)) 
  where
    vtt = TypeTerm (Just v) vte -- An argument variable's name becomes it's type's tag
typeInferIsom g (LambdaTerm ltg (Application fn ag)) = do
  (TypeTerm _ te) <- typeInferIsom' g fn -- Infer type of function
  to <- applyNamedArg g te ag -- Try to apply the function to the argument
  Just (TypeTerm ltg to)
typeInferIsom g (LambdaTerm ltg (Conditional co th el)) =
  typeCheckIsom' g co (TypeConstant BooleanType) -- Is the condition boolean?
    ?>> case typeInferIsom' g th of -- Try infering the type of then-case
      Just t | typeCheckIsom' g el (typeExpr' t) -> Just t -- Does else-case check against it?
      _ -> case typeInferIsom' g el of -- Otherwise, try infering the type of else-case
        Just t | typeCheckIsom' g th (typeExpr' t) -> Just t -- Does then-case check against it?
        _ -> Nothing

typeInferIsom' :: TypingContext TypeTerm -> LambdaExpr -> Maybe TypeTerm
typeInferIsom' g le = typeInferIsom g (LambdaTerm Nothing le)

(<<=) :: Eq a => Maybe a -> Maybe a -> Bool
-- ^ Either the left term should be nothing,
--   or the terms should be equal.
(<<=) x y = isNothing x || x == y

(<:) :: TypeExpr -> TypeExpr -> Bool
-- ^ Could left be used in a place expecting right?
(<:) (TypeConstant bt) (TypeConstant bt') = bt == bt'
(<:) (TypeFunction tt te) (TypeFunction tt' te') = (tt <<: tt') && te == te'
(<:) _ _ = False

(<<:) :: TypeTerm -> TypeTerm -> Bool
-- ^ Could left be used in a place expecting right?
(<<:) (TypeTerm tg te) (TypeTerm tg' te') = tg <<= tg' && te <: te'

applyNamedArg :: TypingContext TypeTerm -> TypeExpr -> LambdaTerm -> Maybe TypeExpr
-- ^ Descend into a given function type and try to find
--   an argument that fits the given lambda term.
--   Return the reduced function expression.
applyNamedArg g (TypeFunction fr to) ag
  | typeCheckIsom g ag fr = Just to -- Does the argument type fit the from type?
  | otherwise = TypeFunction fr <$> applyNamedArg g to ag -- If not, search for fitting argument type further down
applyNamedArg _ _ _ = Nothing

findNamedArg :: TypeExpr -> TypeTerm -> Maybe TypeExpr
-- ^ Descend into a given function type and try to find
--   an argument that fits the given type term.
--   Return the reduced function expression.
findNamedArg (TypeFunction fr to) ag
  | ag <<: fr = Just to
  | otherwise = TypeFunction fr <$> findNamedArg to ag
findNamedArg _ _ = Nothing