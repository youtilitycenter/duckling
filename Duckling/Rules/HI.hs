--Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}

module Duckling.Rules.HI
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Numeral.HI.Rules as Numeral
import qualified Duckling.Ordinal.HI.Rules as Ordinal

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Some Dimension -> [Rule]
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = []
langRules (This Distance) = []
langRules (This Duration) = []
langRules (This Numeral) = Numeral.rules
langRules (This Email) = []
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = []
langRules (This RegexMatch) = []
langRules (This Temperature) = []
langRules (This Time) = []
langRules (This TimeGrain) = []
langRules (This Url) = []
langRules (This Volume) = []
langRules (This Verb) = []
langRules (This Semantic) = []
langRules (This Article) = []
langRules (This Adverb) = []
langRules (This Conjunction) = []
langRules (This Pronoun) = []
langRules (This Preposition) = []
