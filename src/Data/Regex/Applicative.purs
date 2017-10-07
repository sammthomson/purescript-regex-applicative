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
  , RE
  , mkEps
  , mkSymbol
  , mkAlt
  , mkApp
  , mkFmap
  , mkFail
  , mkRep
)
import Data.Regex.Applicative.Interface (
  sym
  , psym
  , msym
  , anySym
  , str
  , few
  , some
  , many
  , withMatched
  , match
  , (=~)
  , replace
  , findFirstPrefix
  , findLongestPrefix
  , findShortestPrefix
  , findFirstInfix
  , findLongestInfix
  , findShortestInfix
)
