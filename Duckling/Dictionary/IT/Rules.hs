-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.IT.Rules
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
import Duckling.Dictionary.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Dictionary.Types (DictionaryData(..))
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Dictionary.Types as TDictionary

-- LANGUAGE
import Duckling.Language.IT.H
import Duckling.Language.IT.V

verbs :: [(Text, String, Text, Text, Text)]
verbs =
  [
  ]
  ++ h
  ++ v

ruleVerbs :: [Rule]
ruleVerbs = map go verbs
  where
    go :: (Text, String, Text, Text, Text) -> Rule
    go (lemma, regexPattern, form, mode, person) = Rule
      { name = lemma
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Dictionary $ verbHelper lemma form mode person
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleVerbs
