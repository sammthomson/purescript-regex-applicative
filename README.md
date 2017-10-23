# purescript-regex-applicative
Regex-based parsing with an applicative interface.

This package is a port of the Haskell
[regex-applicative](https://github.com/feuerbach/regex-applicative) package by
Roman Cheplyaka.
The original MIT licence is preserved for this derivative work.

purescript-regex-applicative is intended to be an efficient and easy to use parsing combinator library for regular expressions in PureScript.


# Usage examples

Here are two example use cases from the test suite:


## Expression example

```purescript
    module Test.Expression where

    import Prelude
    import Control.Alt ((<|>))
    import Data.Array (singleton)
    import Data.Char.Unicode (isAlpha, isAlphaNum, isSpace)
    import Data.Generic (class Generic, gShow)
    import Data.Maybe (Maybe(..))
    import Data.NonEmpty (foldl1, (:|))
    import Data.Regex.Applicative (Re, foldMany, pSingleton, sym, (=~))
    import Data.Regex.Applicative.Common (decimal)
    import Test.Spec (Spec, it)
    import Test.Spec.Assertions (shouldEqual)


    data Lexeme
      = Number Int
      | Op Char
      | Identifier String
      | LParen
      | RParen
    derive instance gLexeme :: Generic Lexeme
    instance showv :: Show Lexeme where show = gShow
    derive instance eqLexeme :: Eq Lexeme

    op :: Re Char Char
    op = foldl1 (<|>) $ map sym $ '*' :| ['/', '-', '+']

    identifier :: Re Char String
    identifier = pSingleton isAlpha <> foldMany (pSingleton isAlphaNum)

    space :: Re Char String
    space = foldMany $ pSingleton isSpace

    lexeme :: Re Char Lexeme
    lexeme = (Number <$> decimal)
              <|> (Op <$> op)
              <|> (Identifier <$> identifier)
              <|> (LParen <$ sym '(')
              <|> (RParen <$ sym ')')

    lexemes :: Re Char (Array Lexeme)
    lexemes = foldMany ((singleton <$> lexeme) <|> ([] <$ space))

    expressionTest :: forall e. Spec e Unit
    expressionTest = it "expression" $
      ("foo + 2*bar - 3/baz" =~ lexemes) `shouldEqual`
        (Just
          [ Identifier "foo"
          , Op '+'
          , Number 2
          , Op '*'
          , Identifier "bar"
          , Op '-'
          , Number 3
          , Op '/'
          , Identifier "baz"
          ])
```

## URL example

```purescript
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
```
