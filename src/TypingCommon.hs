module TypingCommon
  ( TypingContext,
    emptyContext,
    lookupVar,
    pushVar,
    (?>>),
    -- (||=),
    (>>=?),
    (<==),
    Result,
    failure,
    success,
    (|++),
    (///),
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import LambdaTerm
import TypeTerm
import Debug.Trace (traceShow)
import Control.Applicative (Alternative (empty))

(///) :: Show a => b -> a -> b
(///) x y = if debugEnabled then traceShow y x else x
  where
    debugEnabled = True

type TypingContext a = Map.Map String [a]

type Result a = Either String a

failure :: String -> Result a
failure = Left -- /// s

success :: Show a => a -> Result a
success = Right -- /// show x

-- | If Left and Right are failure, prepend Right to Left.
--   Otherwise, return Left.
(|++) :: Result a -> Result b -> Result a
(|++) (Left s) (Left s') = Left (s' ++ "\n" ++ s)
(|++) l _ = l

emptyContext = Map.empty

-- | Find the currently assigned value of the variable
lookupVar :: TypingContext a -> String -> Maybe a
lookupVar g v = head <$> Map.lookup v g

-- | Assign the given value to the variable, shadowing any prior assigned value
pushVar :: TypingContext a -> String -> a -> TypingContext a
pushVar g v t = Map.insert v (t : fromMaybe [] (Map.lookup v g)) g

-- | If the condition is true, return right
--   otherwise return empty
(?>>) :: Alternative f => Bool -> f a -> f a
(?>>) p x = if p then x else empty

-- -- | Return left if Just, otherwise return right
-- (||=) :: Maybe a -> Maybe a -> Maybe a
-- (||=) Nothing d = d
-- (||=) x _ = x

-- | If given Nothing, return False; If given Just, apply function
(>>=?) :: Maybe a -> (a -> Bool) -> Bool
(>>=?) Nothing _ = False
(>>=?) (Just x) f = f x

-- | Either the left term should be nothing,
--   or the terms should be equal.
(<==) :: Eq a => Maybe a -> Maybe a -> Bool
(<==) x y = isNothing x || x == y
