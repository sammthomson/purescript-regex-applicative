-- |
-- Collection of commonly used regular expressions.
module Text.Regex.Applicative.Common (
    -- * Digits
    -- digit
    hexDigit
    -- * Numbers
  , signed
  -- , decimal
  , hexadecimal
) where

-- import Data.Char
import Control.Alt ((<|>))
import Data.Array (some)
import Data.Char.Unicode (digitToInt, isDigit, isHexDigit)
import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import Data.Regex.Applicative (RE, msym, psym, sym)
import Prelude (class Ring, id, negate, pure, (*), (+), (<$), (<$>), (<*>), (<<<))


-- -- | Decimal digit, i.e. @\'0\'@..@\'9\'@
-- digit :: RE Char Int
-- digit = digitToInt <$> psym isDigit

-- | Hexadecimal digit
-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
hexDigit :: RE Char Int
hexDigit = msym digitToInt

-- | Add optional sign
signed :: forall a. Ring a => RE Char a -> RE Char a
signed p = sign <*> p
  where
    sign =  id     <$ sym '+'
        <|> negate <$ sym '-'
        <|> pure id

-- -- | Parse decimal number without sign.
-- decimal :: RE Char Int
-- decimal = foldl (\d i -> d * 10 + i) 0 <$> some digit

-- | Parse decimal number without sign.
hexadecimal :: RE Char Int
hexadecimal = foldl (\d i -> d * 16 + i) 0 <$> some hexDigit
