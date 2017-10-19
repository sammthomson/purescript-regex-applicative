-- adapted from https://github.com/feuerbach/regex-applicative/wiki/Examples
module Test.Expression where

import Control.Alt ((<|>))
import Control.Monad.Eff.Random (RANDOM)
import Data.Char.Unicode (isAlpha, isAlphaNum, isSpace)
import Data.Generic (class Generic, gShow)
import Data.List (List, catMaybes, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (foldl1, (:|))
import Data.Regex.Applicative (Re, foldMany, many, pSingleton, sym, (=~))
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
op = foldl1 (<|>) $ map sym $ '*' :| ['/', '-', '+']

identifier :: Re Char String
identifier = pSingleton isAlpha <> foldMany(pSingleton isAlphaNum)

space :: Re Char String
space = foldMany$ pSingleton isSpace

lexeme :: Re Char Lexeme
lexeme = (Number <$> decimal)
          <|> (Op <$> op)
          <|> (Identifier <$> identifier)
          <|> (LParen <$ sym '(')
          <|> (RParen <$ sym ')')

lexemes :: Re Char (List Lexeme)
lexemes = catMaybes <$> many ((Just <$> lexeme) <|> (Nothing <$ space))

expressionTests :: forall e. Spec (random :: RANDOM | e) Unit
expressionTests = do
  it "fixture" $ quickCheck' 1 $ \(_ :: Unit) ->
    ("a + 2*b - 3/c" =~ lexemes) ==?
      (Just $ fromFoldable
        [ Identifier "a"
        , Op '+'
        , Number 2
        , Op '*'
        , Identifier "b"
        , Op '-'
        , Number 3
        , Op '/'
        , Identifier "c"
        ])
