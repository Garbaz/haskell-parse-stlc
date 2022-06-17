{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Metar where

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

data Report = Report
  { station :: String,
    time :: (Int, Int, Int),
    wind :: WindInfo
  }
  deriving (Show)

data WindInfo = WindInfo
  { dir :: Int,
    speed :: Int,
    gusts :: Maybe Int
  }
  deriving (Show)

vowel :: ReadP Char
vowel = satisfy (`elem` "aouei")

atLeastOneVowel = many1 vowel

airport :: ReadP String
airport = do
  code <- many1 (satisfy (\c -> 'A' <= c && c <= 'Z'))
  satisfy (== ' ')
  return code

digit :: ReadP Char
digit = satisfy (\c -> '0' <= c && c <= '9')

numbers :: Int -> ReadP Int
numbers num_digits = read <$> count num_digits digit

timestamp :: ReadP (Int, Int, Int)
timestamp = do
  day <- numbers 2
  hour <- numbers 2
  minute <- numbers 2
  string "Z "
  if day < 1 || day > 31 || hour > 23 || minute > 59
    then pfail
    else return (day, hour, minute)

gust :: ReadP Int
gust = do
  satisfy (== 'G')
  numbers 2 <|> numbers 3

windInfo :: ReadP WindInfo
windInfo = do
  direction <- numbers 3
  speed <- numbers 2 <|> numbers 3
  gusts <- option Nothing (Just <$> gust)
  unit <- string "KT" <|> string "MPS"
  string " "
  return (WindInfo direction (toMPS unit speed) (toMPS unit <$> gusts))
  where
    toMPS :: String -> Int -> Int
    toMPS "KT" speed = speed `div` 2
    toMPS _ speed = speed

metar :: ReadP Report
metar = do
  code <- airport
  time <- timestamp
  wind <- windInfo
  return (Report code time wind)

