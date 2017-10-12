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
  Greediness(..)
  , Re
)
import Data.Regex.Applicative.Interface (
  -- functions on Strings
  (=~)
  , sym
  , psym
  , msym
  , anySym
  , str
  , few
  , findFirstPrefix
  , findLongestPrefix
  , findShortestPrefix
  , findFirstInfix
  , findLongestInfix
  , findShortestInfix
  , many
  , match
  , matchFlipped
  , replace
  , some
  , withMatched
  -- functions on sequences
  , (=~~)
  , sym'
  , psym'
  , anySym'
  , seq
  , few'
  , findFirstPrefix'
  , findLongestPrefix'
  , findShortestPrefix'
  , findFirstInfix'
  , findLongestInfix'
  , findShortestInfix'
  , many'
  , match'
  , matchFlipped'
  , replace'
  , some'
  , withMatched'
)
