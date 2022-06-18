-- A few generic parsing utilities used by other parts of the code.

module ParsingCommon where

import Text.ParserCombinators.ReadP

lowercase :: ReadP Char
lowercase = satisfy (\c -> 'a' <= c && c <= 'z')

uppercase :: ReadP Char
uppercase = satisfy (\c -> 'A' <= c && c <= 'Z')

numeral :: ReadP Int
numeral = read <$> many1 (satisfy (\c -> '0' <= c && c <= '9'))

capitalized :: ReadP String
capitalized = do
  hd <- uppercase
  tl <- many lowercase
  return (hd : tl)

bracketed :: ReadP a -> ReadP a
bracketed = between (char '(') (char ')')

perhaps :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
-- ^ Tries first to parse the first argument applied to the second (@o p@)
--   and if that fails, tries the second argument on it's own (@p@).
--
--   Example:
--
--       @readP_to_S (perhaps bracketed (char 'x')) "(x)"  -- => 'x'@
--
--       @readP_to_S (perhaps bracketed (char 'x')) "x"    -- => 'x'@
--
--   (with @bracketed = between (char '(') (char ')')@)
perhaps o p = o p <++ p