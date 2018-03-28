-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Quantity.IT.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Data.Text as T
import Prelude as P
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Quantity.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Quantity.Types (QuantityData(..))
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

quantities :: [(Text, String, TQuantity.Unit)]
quantities =
  [ ("<number> pezzi di product", "(?:pezzo|pezzi|tipi|tipo) di (\\w+)", (TQuantity.Custom "type_of_generic_product"))
  , ("<number> product", "((?(?!pezzo|pezzi|tipi|tipo)\\w+))", (TQuantity.Custom "generic_product"))
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "milligrammo" , (/ 1000))
  , ( "milligrammi" , (/ 1000))
  , ( "mg"          , (/ 1000))
  , ( "mgs"         , (/ 1000))
  , ( "kilo"        , (* 1000))
  , ( "kili"        , (* 1000))
  , ( "kg"          , (* 1000))
  , ( "kgs"         , (* 1000))
  ]

getValue :: Text -> Double -> Double
getValue match = HashMap.lookupDefault id (Text.toLower match) opsMap

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = P.map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [Predicate isPositive, regex regexPattern]
      , prod = \case
        (Token Numeral nd:
         Token RegexMatch (GroupMatch (product:_)):
         _) -> Just . Token Quantity $ quantityProduct u value (T.strip product)
          where value = getValue (T.strip product) $ TNumeral.value nd
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleNumeralQuantities
