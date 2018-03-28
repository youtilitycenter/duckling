-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.Adjective.IT.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Dictionary.Adjective.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Dictionary.Adjective.Types (AdjectiveData(..))
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Dictionary.Adjective.Types as TAdjective

adjectives :: [(Text, String, TAdjective.Adjective)]
adjectives =
  [ ("il", "(il)", TAdjective.Il)
  , ("lo", "(lo)", TAdjective.Lo)
  , ("l'", "(l'|l)", TAdjective.L)
  , ("i", "(i)", TAdjective.I)
  , ("gli", "(gli)", TAdjective.Gli)
  , ("la", "(la)", TAdjective.La)
  , ("le", "(le)", TAdjective.Le)
  , ("un", "(un)", TAdjective.Un)
  , ("uno", "(uno)", TAdjective.Uno)
  , ("una", "(una|un')", TAdjective.Una)
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go adjectives
  where
    go :: (Text, String, TAdjective.Adjective) -> Rule
    go (name, regexPattern, a) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Adjective $ adjectiveHelper a
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleNumeralQuantities
