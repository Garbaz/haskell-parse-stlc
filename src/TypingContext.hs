module TypingContext
  ( TypingContext,
    emptyContext,
    lookupVar,
    pushVar,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import TypeTerm

type TypingContext = Map.Map String [TypeExpr]

emptyContext = Map.empty

lookupVar :: TypingContext -> String -> Maybe TypeExpr
lookupVar g v = head <$> Map.lookup v g

pushVar :: TypingContext -> String -> TypeExpr -> TypingContext
pushVar g v t = Map.insert v (t : fromMaybe [] (Map.lookup v g)) g
