module ParsingCommon where

import Text.ParserCombinators.ReadP

lowerCase :: ReadP Char
lowerCase = satisfy (\c -> 'a' <= c && c <= 'z')

upperCase :: ReadP Char
upperCase = satisfy (\c -> 'A' <= c && c <= 'Z')

capitalized :: ReadP String
capitalized = do
  hd <- upperCase
  tl <- many lowerCase
  return (hd : tl)

bracketed :: ReadP a -> ReadP a
bracketed = between (char '(') (char ')')

perhaps :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
perhaps o p = o p <++ p