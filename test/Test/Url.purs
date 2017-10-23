--------------------------------------------------------------------
-- |
-- | Module    : Test.Url
-- | Copyright : (c) Sam Thomson, 2017
-- | Copyright : (c) Roman Cheplyaka, 2011
-- | License   : MIT
-- |
-- | Test adapted from http://stackoverflow.com/questions/7882512/pcre-in-haskell-what-where-how/7902344#7902344.
--------------------------------------------------------------------
module Test.Url where

import Prelude
import Control.Alt ((<|>))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Regex.Applicative (Re, anySingleton, foldMany, pSingleton, str, sym, (=~))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)


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
host = foldMany $ pSingleton $ ((/=) '/')

url :: Re Char Url
url = Url <$> protocol <* str "://" <*> host <* sym '/' <*> foldMany anySingleton

urlTest :: forall e. Spec e Unit
urlTest = it "url" $
  ("http://stackoverflow.com/questions" =~ url) `shouldEqual`
    Just (Url Http "stackoverflow.com" "questions")
