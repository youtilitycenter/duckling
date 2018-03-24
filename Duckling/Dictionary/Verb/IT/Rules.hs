-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.Verb.IT.Rules
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
import Duckling.Dictionary.Verb.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Dictionary.Verb.Types (VerbData(..))
import qualified Duckling.Numeral.Types as TNumeral

-- LANGUAGE
import Duckling.Dictionary.Verb.IT.Values

verbs :: [(Text, String)]
verbs =
  [
  ]
  ++ verbsValue

ruleVerbs :: [Rule]
ruleVerbs = map go verbs
  where
    go :: (Text, String) -> Rule
    go (lemma, regexPattern) = Rule
      { name = lemma
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (verb:special:final:_ )):
         _) -> case Text.toLower special of

           "e" -> case Text.toLower final of
             "rò" -> Just . Token Verb $ verbHelper lemma "future" "1s"
             "rai" -> Just . Token Verb $ verbHelper lemma "future" "2s"
             "rà" -> Just . Token Verb $ verbHelper lemma "future" "3s"
             "remo" -> Just . Token Verb $ verbHelper lemma "future" "1p"
             "rete" -> Just . Token Verb $ verbHelper lemma "future" "2p"
             "ranno" -> Just . Token Verb $ verbHelper lemma "future" "3p"

             "vo" -> Just . Token Verb $ verbHelper lemma "imperfect" "1s"
             "vi" -> Just . Token Verb $ verbHelper lemma "imperfect" "2s"
             "va" -> Just . Token Verb $ verbHelper lemma "imperfect" "3s"
             "vamo" -> Just . Token Verb $ verbHelper lemma "imperfect" "1p"
             "vate" -> Just . Token Verb $ verbHelper lemma "imperfect" "2p"
             "vano" -> Just . Token Verb $ verbHelper lemma "imperfect" "3p"

             "rei" -> Just . Token Verb $ verbHelper lemma "present" "1s"
             "resti" -> Just . Token Verb $ verbHelper lemma "present" "2s"
             "rebbe" -> Just . Token Verb $ verbHelper lemma "present" "3s"
             "remmo" -> Just . Token Verb $ verbHelper lemma "present" "1p"
             "reste" -> Just . Token Verb $ verbHelper lemma "present" "2p"
             "rebbero" -> Just . Token Verb $ verbHelper lemma "present" "3p"
             _ -> Nothing

           "ò" -> Just . Token Verb $ verbHelper lemma "past" "3s"

           "a" -> case Text.toLower final of
             "i" -> Just . Token Verb $ verbHelper lemma "past" "1s"
             "sti" -> Just . Token Verb $ verbHelper lemma "past" "2s"
             "mmo" -> Just . Token Verb $ verbHelper lemma "past" "1p"
             "ste" -> Just . Token Verb $ verbHelper lemma "past" "2p"
             "rono" -> Just . Token Verb $ verbHelper lemma "past" "3p"
             "te" -> Just . Token Verb $ verbHelper lemma "present" "2p"
             "no" -> Just . Token Verb $ verbHelper lemma "present" "3p"

             "to" -> Just . Token Verb $ verbHelper lemma "past" "nothing"

             "vo" -> Just . Token Verb $ verbHelper lemma "imperfect" "1s"
             "vi" -> Just . Token Verb $ verbHelper lemma "imperfect" "2s"
             "va" -> Just . Token Verb $ verbHelper lemma "imperfect" "3s"
             "vamo" -> Just . Token Verb $ verbHelper lemma "imperfect" "1p"
             "vate" -> Just . Token Verb $ verbHelper lemma "imperfect" "2p"
             "vano" -> Just . Token Verb $ verbHelper lemma "imperfect" "3p"

             "ssi" -> Just . Token Verb $ verbHelper lemma "imperfect" "nothing"
             "sse" -> Just . Token Verb $ verbHelper lemma "imperfect" "3s"
             "ssimo" -> Just . Token Verb $ verbHelper lemma "imperfect" "1p"
             "ssero" -> Just . Token Verb $ verbHelper lemma "imperfect" "3p"

             "nte" -> Just . Token Verb $ verbHelper lemma "present" "nothing"
             "ndo" -> Just . Token Verb $ verbHelper lemma "present" "nothing"
             _ -> Just . Token Verb $ verbHelper lemma "present" "3s"

           "o" -> Just . Token Verb $ verbHelper lemma "present" "1s"

           "i" -> case Text.toLower final of
             "vo" -> Just . Token Verb $ verbHelper lemma "imperfect" "1s"
             "vi" -> Just . Token Verb $ verbHelper lemma "imperfect" "2s"
             "va" -> Just . Token Verb $ verbHelper lemma "imperfect" "3s"
             "vamo" -> Just . Token Verb $ verbHelper lemma "imperfect" "1p"
             "vate" -> Just . Token Verb $ verbHelper lemma "imperfect" "2p"
             "vano" -> Just . Token Verb $ verbHelper lemma "imperfect" "3p"

             "amo" -> Just . Token Verb $ verbHelper lemma "present" "1p"
             "ate" -> Just . Token Verb $ verbHelper lemma "present" "2p"
             "no" -> Just . Token Verb $ verbHelper lemma "present" "3p"
             _ -> Just . Token Verb $ verbHelper lemma "present" "nothing"

           _ -> Just . Token Verb $ verbHelper lemma "infinite" "nothing"
        _ -> Nothing
      }



rules :: [Rule]
rules =
  [
  ]
  ++ ruleVerbs
