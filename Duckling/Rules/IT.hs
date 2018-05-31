-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.IT
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Duration.IT.Rules as Duration
import qualified Duckling.Email.IT.Rules as Email
import qualified Duckling.Dictionary.Verb.IT.Rules as Verb
import qualified Duckling.Dictionary.Semantic.IT.Rules as Semantic
import qualified Duckling.Dictionary.SemanticNP.IT.Rules as SemanticNP
import qualified Duckling.Dictionary.SemanticVP.IT.Rules as SemanticVP
import qualified Duckling.Dictionary.SemanticPP.IT.Rules as SemanticPP
import qualified Duckling.Dictionary.Article.IT.Rules as Article
import qualified Duckling.Dictionary.Adverb.IT.Rules as Adverb
import qualified Duckling.Dictionary.Adjective.IT.Rules as Adjective
import qualified Duckling.Dictionary.Conjunction.IT.Rules as Conjunction
import qualified Duckling.Dictionary.Pronoun.IT.Rules as Pronoun
import qualified Duckling.Dictionary.Preposition.IT.Rules as Preposition
import qualified Duckling.Numeral.IT.Rules as Numeral
import qualified Duckling.Ordinal.IT.Rules as Ordinal
import qualified Duckling.Quantity.IT.Rules as Quantity
import qualified Duckling.Temperature.IT.Rules as Temperature
import qualified Duckling.Time.IT.Rules as Time
import qualified Duckling.TimeGrain.IT.Rules as TimeGrain
import qualified Duckling.Volume.IT.Rules as Volume

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Some Dimension -> [Rule]
localeRules region (This (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = []
langRules (This Distance) = []
langRules (This Duration) = Duration.rules
langRules (This Email) = Email.rules
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = Quantity.rules
langRules (This RegexMatch) = []
langRules (This Temperature) = Temperature.rules
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = Volume.rules
langRules (This Verb) = Verb.rules
langRules (This Semantic) = Semantic.rules
langRules (This SemanticNP) = SemanticNP.rules
langRules (This SemanticVP) = SemanticVP.rules
langRules (This SemanticPP) = SemanticPP.rules
langRules (This Article) = Article.rules
langRules (This Adverb) = Adverb.rules
langRules (This Conjunction) = Conjunction.rules
langRules (This Pronoun) = Pronoun.rules
langRules (This Preposition) = Preposition.rules
langRules (This Adjective) = Adjective.rules
langRules (This (CustomDimension dim)) = dimLangRules IT dim
