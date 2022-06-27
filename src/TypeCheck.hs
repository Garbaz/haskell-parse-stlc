module TypeCheck
--   ( typeCheck,
--   )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import LambdaTerm
import TypeTerm

type TypingContext = Map.Map String TypeTerm

-- eqThenJust :: Eq a => a -> a -> Maybe a
-- eqThenJust x x' = if x == x' then Just x else Nothing

-- typeCheck :: TypingContext -> LambdaTerm -> TypeTerm -> Maybe TypeTerm
-- typeCheck g (Variable vname) t = Map.lookup vname g >>= eqThenJust t
-- typeCheck g (Constant const) t =
--   if isNothing (typeTag' t) && typeExpr' t == typeOfConst const
--     then Just t
--     else Nothing
-- typeCheck g (Abstraction vname annot body) t@(TypeTerm tag (TypeFunction from to)) = do
--   typeCheck g (Variable vname) from
--   return t
-- typeCheck _ _ _ = Nothing