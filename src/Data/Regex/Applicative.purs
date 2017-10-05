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
  module Data.Regex.Applicative.Types,
  module Data.Regex.Applicative.Interface
) where

import Data.Regex.Applicative.Types (
  Greediness(..),
  RE,
  eps,
  symbol,
  alt,
  app,
  fmap,
  fail,
  rep,
  void
)
import Data.Regex.Applicative.Interface (
  sym
  , psym
  , msym
  , anySym
  , string
  , reFoldl
  , few
--   , comap
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
-- import Control.Applicative
