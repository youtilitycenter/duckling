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
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = []
langRules (This Distance) = []
langRules (This Duration) = Duration.rules
langRules (This Email) = Email.rules
langRules (This Verb) = Verb.rules
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
