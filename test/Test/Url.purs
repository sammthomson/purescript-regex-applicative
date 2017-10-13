-- | Adapted from http://stackoverflow.com/questions/7882512/pcre-in-haskell-what-where-how/7902344#7902344.
module Test.Url where

import Control.Alt ((<|>))
import Control.Monad.Eff.Random (RANDOM)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Regex.Applicative (Re, anySym, many, psym, str, sym, (=~))
import Prelude (class Eq, class Show, Unit, ($), (/=), (<$), (<$>), (<*), (<*>))
import Test.QuickCheck ((==?))
import Test.Spec (Spec, it)
import Test.Spec.QuickCheck (quickCheck')


data Protocol = Http | Ftp

derive instance eqProtocol :: Eq Protocol
derive instance gProtocol :: Generic Protocol
instance showProtocol :: Show Protocol where show = gShow

protocol :: Re Char Protocol
protocol = Http <$ str "http" <|> Ftp <$ str "ftp"

type Host = String

type Location = String

data Url = Url Protocol Host Location

derive instance eqUrl :: Eq Url
derive instance gUrl :: Generic Url
instance showUrl :: Show Url where show = gShow

host :: Re Char Host
host = many $ psym $ ((/=) '/')

url :: Re Char Url
url = Url <$> protocol <* str "://" <*> host <* sym '/' <*> many anySym

urlTest :: forall e. Spec (random :: RANDOM | e) Unit
urlTest =
  it "url" $ quickCheck' 1 $
    ("http://stackoverflow.com/questions" =~ url) ==?
      (Just (Url Http "stackoverflow.com" "questions"))
