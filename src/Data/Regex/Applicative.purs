--------------------------------------------------------------------
-- |
-- | Module    : Data.Regex.Applicative
-- | Copyright : (c) Sam Thomson, 2017
-- | Copyright : (c) Roman Cheplyaka, 2011
-- | License   : MIT
-- |
--------------------------------------------------------------------
module Data.Regex.Applicative (
  module Data.Regex.Applicative.Types
  , module Data.Regex.Applicative.Interface
) where

import Data.Regex.Applicative.Types (
  Re
)
import Data.Regex.Applicative.Interface (
  InfixMatch(..)
  , (=~)
  , anySingleton
  , anySym
  , few
  , findFirstPrefix
  , findLongestPrefix
  , findShortestPrefix
  , findFirstInfix
  , findLongestInfix
  , findShortestInfix
  , foldFew
  , foldMany
  , foldSome
  , many
  , match
  , matchFlipped
  , msym
  , pSingleton
  , psym
  , replace
  , singleton
  , str
  , some
  , sym
  , withMatched
)
