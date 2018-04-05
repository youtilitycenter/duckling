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
import Duckling.Dictionary.Verb.IT.Values.Regular.A as Regular_A
import Duckling.Dictionary.Verb.IT.Values.Regular.B as Regular_B
import Duckling.Dictionary.Verb.IT.Values.Regular.C as Regular_C
import Duckling.Dictionary.Verb.IT.Values.Regular.D as Regular_D
import Duckling.Dictionary.Verb.IT.Values.Regular.E as Regular_E
import Duckling.Dictionary.Verb.IT.Values.Regular.F as Regular_F
import Duckling.Dictionary.Verb.IT.Values.Regular.G as Regular_G
import Duckling.Dictionary.Verb.IT.Values.Regular.H as Regular_H
import Duckling.Dictionary.Verb.IT.Values.Regular.I as Regular_I
import Duckling.Dictionary.Verb.IT.Values.Regular.L as Regular_L
import Duckling.Dictionary.Verb.IT.Values.Regular.M as Regular_M
import Duckling.Dictionary.Verb.IT.Values.Regular.N as Regular_N
import Duckling.Dictionary.Verb.IT.Values.Regular.O as Regular_O
import Duckling.Dictionary.Verb.IT.Values.Regular.P as Regular_P
import Duckling.Dictionary.Verb.IT.Values.Regular.Q as Regular_Q
import Duckling.Dictionary.Verb.IT.Values.Regular.R as Regular_R
import Duckling.Dictionary.Verb.IT.Values.Regular.S as Regular_S
import Duckling.Dictionary.Verb.IT.Values.Regular.T as Regular_T
import Duckling.Dictionary.Verb.IT.Values.Regular.U as Regular_U
import Duckling.Dictionary.Verb.IT.Values.Regular.V as Regular_V
import Duckling.Dictionary.Verb.IT.Values.Regular.Z as Regular_Z

import Duckling.Dictionary.Verb.IT.Values.Irregular.A as Irregular_A
import Duckling.Dictionary.Verb.IT.Values.Irregular.B as Irregular_B
import Duckling.Dictionary.Verb.IT.Values.Irregular.C as Irregular_C
import Duckling.Dictionary.Verb.IT.Values.Irregular.D as Irregular_D
import Duckling.Dictionary.Verb.IT.Values.Irregular.E as Irregular_E
import Duckling.Dictionary.Verb.IT.Values.Irregular.F as Irregular_F
import Duckling.Dictionary.Verb.IT.Values.Irregular.G as Irregular_G
import Duckling.Dictionary.Verb.IT.Values.Irregular.H as Irregular_H
import Duckling.Dictionary.Verb.IT.Values.Irregular.I as Irregular_I
import Duckling.Dictionary.Verb.IT.Values.Irregular.L as Irregular_L
import Duckling.Dictionary.Verb.IT.Values.Irregular.M as Irregular_M
import Duckling.Dictionary.Verb.IT.Values.Irregular.N as Irregular_N
import Duckling.Dictionary.Verb.IT.Values.Irregular.O as Irregular_O
import Duckling.Dictionary.Verb.IT.Values.Irregular.P as Irregular_P
import Duckling.Dictionary.Verb.IT.Values.Irregular.R as Irregular_R
import Duckling.Dictionary.Verb.IT.Values.Irregular.S as Irregular_S
import Duckling.Dictionary.Verb.IT.Values.Irregular.T as Irregular_T
import Duckling.Dictionary.Verb.IT.Values.Irregular.U as Irregular_U
import Duckling.Dictionary.Verb.IT.Values.Irregular.V as Irregular_V

regularVerbsValue :: [(Text, String)]
regularVerbsValue =
  [
  ]
  ++ Regular_A.verbs
  ++ Regular_B.verbs
  ++ Regular_C.verbs
  ++ Regular_D.verbs
  ++ Regular_E.verbs
  ++ Regular_F.verbs
  ++ Regular_G.verbs
  ++ Regular_H.verbs
  ++ Regular_I.verbs
  ++ Regular_L.verbs
  ++ Regular_M.verbs
  ++ Regular_N.verbs
  ++ Regular_O.verbs
  ++ Regular_P.verbs
  ++ Regular_Q.verbs
  ++ Regular_R.verbs
  ++ Regular_S.verbs
  ++ Regular_T.verbs
  ++ Regular_U.verbs
  ++ Regular_V.verbs
  ++ Regular_Z.verbs

irregularVerbsValue :: [(Text, String, Text, Text)]
irregularVerbsValue =
  [
  ]
  ++ Irregular_A.verbs
  ++ Irregular_B.verbs
  ++ Irregular_C.verbs
  ++ Irregular_D.verbs
  ++ Irregular_E.verbs
  ++ Irregular_F.verbs
  ++ Irregular_G.verbs
  ++ Irregular_H.verbs
  ++ Irregular_I.verbs
  ++ Irregular_L.verbs
  ++ Irregular_M.verbs
  ++ Irregular_N.verbs
  ++ Irregular_O.verbs
  ++ Irregular_P.verbs
  ++ Irregular_R.verbs
  ++ Irregular_S.verbs
  ++ Irregular_T.verbs
  ++ Irregular_U.verbs
  ++ Irregular_V.verbs

ruleFindRegularVerbs :: [Rule]
ruleFindRegularVerbs = map go regularVerbsValue
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

ruleFindIrregularVerbs :: [Rule]
ruleFindIrregularVerbs = map go irregularVerbsValue
  where
    go :: (Text, String, Text, Text) -> Rule
    go (lemma, regexPattern, form, person) = Rule
      { name = lemma
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Verb $ verbHelper lemma form person
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleFindRegularVerbs
  ++ ruleFindIrregularVerbs
