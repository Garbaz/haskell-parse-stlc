module TypingCommon
  ( TypingContext,
    emptyContext,
    lookupVar,
    pushVar,
    (?>>),
    (||=),
    (>>=?),
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import TypeTerm

type TypingContext a = Map.Map String [a]

emptyContext = Map.empty

lookupVar :: TypingContext a -> String -> Maybe a
lookupVar g v = head <$> Map.lookup v g

pushVar :: TypingContext a -> String -> a -> TypingContext a
pushVar g v t = Map.insert v (t : fromMaybe [] (Map.lookup v g)) g

(?>>) :: Bool -> Maybe a -> Maybe a
-- ^ If the condition is true, pass through
--   otherwise return nothing
(?>>) p x = if p then x else Nothing

(||=) :: Maybe a -> Maybe a -> Maybe a
-- ^ Return left unless Nothing, then return right
(||=) Nothing d = d
(||=) x _ = x

(>>=?) :: Maybe a -> (a -> Bool) -> Bool
-- ^ If given Nothing, return False; If given Just, apply function
(>>=?) Nothing _ = False
(>>=?) (Just x) f = f x