-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.SemanticPP.IT.Rules
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
import Duckling.Dictionary.SemanticPP.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral

rule_PP__IN_NP :: Rule
rule_PP__IN_NP = Rule
  { name = "<in> <article> noun"
  , pattern =
    [ dimension Preposition
    , dimension Article
    , regex "(\\w+)"
    ]
  , prod = \case
    (Token Preposition p:Token Article a:Token RegexMatch (GroupMatch (noun:_)):_) ->
      Just . Token SemanticPP $ semanticHelper_PP__IN_NP p a noun
    _ -> Nothing
  }

rule_PP__IN_DT_NN :: Rule
rule_PP__IN_DT_NN = Rule
  { name = "<in> <number> noun"
  , pattern =
    [ dimension Preposition
    , dimension Numeral
    , regex "(\\w+)"
    ]
  , prod = \case
    (Token Preposition p:Token Numeral n:Token RegexMatch (GroupMatch (noun:_)):_) ->
      Just . Token SemanticPP $ semanticHelper_PP__IN_DT_NN p n noun
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ rule_PP__IN_NP
  , rule_PP__IN_DT_NN
  ]
