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

-- LANGUAGE
import Duckling.Dictionary.Adjective.IT.Values.A as A
import Duckling.Dictionary.Adjective.IT.Values.B as B
import Duckling.Dictionary.Adjective.IT.Values.C as C
import Duckling.Dictionary.Adjective.IT.Values.D as D
import Duckling.Dictionary.Adjective.IT.Values.E as E
import Duckling.Dictionary.Adjective.IT.Values.F as F
import Duckling.Dictionary.Adjective.IT.Values.G as G
import Duckling.Dictionary.Adjective.IT.Values.H as H
import Duckling.Dictionary.Adjective.IT.Values.I as I
import Duckling.Dictionary.Adjective.IT.Values.L as L
import Duckling.Dictionary.Adjective.IT.Values.M as M
import Duckling.Dictionary.Adjective.IT.Values.N as N
import Duckling.Dictionary.Adjective.IT.Values.O as O
import Duckling.Dictionary.Adjective.IT.Values.P as P
import Duckling.Dictionary.Adjective.IT.Values.Q as Q
import Duckling.Dictionary.Adjective.IT.Values.R as R
import Duckling.Dictionary.Adjective.IT.Values.S as S
import Duckling.Dictionary.Adjective.IT.Values.T as T
import Duckling.Dictionary.Adjective.IT.Values.U as U
import Duckling.Dictionary.Adjective.IT.Values.V as V
import Duckling.Dictionary.Adjective.IT.Values.Z as Z

adjectivesValue :: [(Text, String)]
adjectivesValue =
  [
  ]
  ++ A.adjectives
  ++ B.adjectives
  ++ C.adjectives
  ++ D.adjectives
  ++ E.adjectives
  ++ F.adjectives
  ++ G.adjectives
  ++ H.adjectives
  ++ I.adjectives
  ++ L.adjectives
  ++ M.adjectives
  ++ N.adjectives
  ++ O.adjectives
  ++ P.adjectives
  ++ Q.adjectives
  ++ R.adjectives
  ++ S.adjectives
  ++ T.adjectives
  ++ U.adjectives
  ++ V.adjectives
  ++ Z.adjectives

ruleFindAdjectives :: [Rule]
ruleFindAdjectives = map go adjectivesValue
  where
    go :: (Text, String) -> Rule
    go (lemma, regexPattern) = Rule
      { name = lemma
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Adjective $ adjectiveHelper lemma
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleFindAdjectives
