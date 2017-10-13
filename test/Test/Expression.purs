-- adapted from https://github.com/feuerbach/regex-applicative/wiki/Examples
module Test.Expression where

import Control.Alt ((<|>))
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (catMaybes)
import Data.Char.Unicode (isAlpha, isAlphaNum, isSpace)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (foldl1, (:|))
import Data.Regex.Applicative (Re, many, many', psym, sym, sym', (=~))
import Data.Regex.Applicative.Common (decimal)
import Prelude (class Eq, class Show, Unit, map, ($), (<$), (<$>), (<>))
import Test.QuickCheck ((==?))
import Test.Spec (Spec, it)
import Test.Spec.QuickCheck (quickCheck')


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
op = foldl1 (<|>) $ map sym' $ '*' :| ['/', '-', '+']

identifier :: Re Char String
identifier = psym isAlpha <> many (psym isAlphaNum)

space :: Re Char String
space = many $ psym isSpace

lexeme :: Re Char Lexeme
lexeme = (Number <$> decimal)
          <|> (Op <$> op)
          <|> (Identifier <$> identifier)
          <|> (LParen <$ sym '(')
          <|> (RParen <$ sym ')')

lexemes :: Re Char (Array Lexeme)
lexemes = catMaybes <$> many' ((Just <$> lexeme) <|> (Nothing <$ space))

expressionTests :: forall e. Spec (random :: RANDOM | e) Unit
expressionTests =
  it "fixture" $ quickCheck' 1 $ \(_ :: Unit) ->
    ("a + 2*b - 3/c" =~ lexemes) ==?
      Just
        [ Identifier "a"
        , Op '+'
        , Number 2
        , Identifier "b"
        , Op '-'
        , Number 3
        , Op '/'
        , Identifier "c"
        ]
