--------------------------------------------------------------------
-- |
-- Module    : Data.Regex.Applicative
-- Copyright : (c) Roman Cheplyaka
-- License   : MIT
--
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
-- Stability : experimental
--
-- To get started, see some examples on the wiki:
-- <https://github.com/feuerbach/regex-applicative/wiki/Examples>
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
