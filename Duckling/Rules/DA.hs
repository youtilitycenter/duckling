-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.DA
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Duration.DA.Rules as Duration
import qualified Duckling.Numeral.DA.Rules as Numeral
import qualified Duckling.Ordinal.DA.Rules as Ordinal
import qualified Duckling.Time.DA.Rules as Time
import qualified Duckling.TimeGrain.DA.Rules as TimeGrain

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Some Dimension -> [Rule]
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = []
langRules (This Distance) = []
langRules (This Duration) = Duration.rules
langRules (This Email) = []
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = []
langRules (This RegexMatch) = []
langRules (This Temperature) = []
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = []
langRules (This Verb) = []
langRules (This Semantic) = []
langRules (This SemanticNP) = []
langRules (This SemanticVP) = []
langRules (This SemanticPP) = []
langRules (This Article) = []
langRules (This Adverb) = []
langRules (This Conjunction) = []
langRules (This Pronoun) = []
langRules (This Preposition) = []
langRules (This Adjective) = []
