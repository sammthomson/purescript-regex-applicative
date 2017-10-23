--------------------------------------------------------------------
-- |
-- | Module    : Test.Expression
-- | Copyright : (c) Sam Thomson, 2017
-- | Copyright : (c) Roman Cheplyaka, 2011
-- | License   : MIT
-- |
-- | Test adapted from https://github.com/feuerbach/regex-applicative/wiki/Examples
--------------------------------------------------------------------
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
