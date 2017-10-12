-- | Collection of commonly used regular expressions.
module Data.Regex.Applicative.Common (
  -- * Digits
  digit,
  hexDigit,
  -- * Numbers
  signed,
  decimal,
  hexadecimal
) where

import Control.Alt ((<|>))
import Data.Char.Unicode (digitToInt, isDigit)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Regex.Applicative (Re, msym, some, sym)
import Prelude (class Ring, id, negate, pure, (*), (+), (<$), (<$>), (<*>))


-- | Decimal digit, i.e. @\'0\'@..@\'9\'@
digit :: Re Char Int
digit = msym (\c -> if isDigit c then digitToInt c else Nothing)

-- | Hexadecimal digit
-- | i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
hexDigit :: Re Char Int
hexDigit = msym digitToInt

-- | Add optional sign
signed :: forall a. Ring a => Re Char a -> Re Char a
signed p = sign <*> p
  where
    sign =  id     <$ sym '+'
        <|> negate <$ sym '-'
        <|> pure id

-- | Parse decimal number without sign.
decimal :: Re Char Int
decimal = foldl (\d i -> d * 10 + i) 0 <$> some digit

-- | Parse decimal number without sign.
hexadecimal :: Re Char Int
hexadecimal = foldl (\d i -> d * 16 + i) 0 <$> some hexDigit
