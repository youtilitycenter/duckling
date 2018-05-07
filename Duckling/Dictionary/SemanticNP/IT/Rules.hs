-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.SemanticNP.IT.Rules
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
import Duckling.Dictionary.SemanticNP.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral

rule_NP__NU_NN :: Rule
rule_NP__NU_NN = Rule
  { name = "<number> noun"
  , pattern =
    [ dimension Numeral
    , regex "(\\w+)"
    ]
  , prod = \case
    (Token Numeral num:Token RegexMatch (GroupMatch (noun:_)):_) ->
      Just . Token SemanticNP $ semanticHelper_NU_NN num noun
    _ -> Nothing
  }

rule_NP__DT_NN :: Rule
rule_NP__DT_NN = Rule
  { name = "<article> noun"
  , pattern =
    [ dimension Article
    , regex "(\\w+)"
    ]
  , prod = \case
    (Token Article a:Token RegexMatch (GroupMatch (noun:_)):_) ->
      Just . Token SemanticNP $ semanticHelper_DT_NN a noun
    _ -> Nothing
  }

rule_NP__NP_PP :: Rule
rule_NP__NP_PP = Rule
  { name = "<np> <pp>"
  , pattern =
    [ dimension SemanticNP
    , dimension SemanticPP
    ]
  , prod = \case
    (Token SemanticNP np:Token SemanticPP pp:_) ->
      Just . Token SemanticNP $ semanticHelper_NP_PP np pp
    _ -> Nothing
  }

rule_NP__DT_ADJ_NN :: Rule
rule_NP__DT_ADJ_NN = Rule
  { name = "<article> <adjective> noun"
  , pattern =
    [ dimension Article
    , dimension Adjective
    , regex "(\\w+)"
    ]
  , prod = \case
    (Token Article a:Token Adjective adj:Token RegexMatch (GroupMatch (noun:_)):_) ->
      Just . Token SemanticNP $ semanticHelper_DT_NN_ADJ a noun adj
    _ -> Nothing
  }

rule_NP__DT_NN_ADJ :: Rule
rule_NP__DT_NN_ADJ = Rule
  { name = "<article> noun <adjective>"
  , pattern =
    [ dimension Article
    , regex "(\\w+)"
    , dimension Adjective
    ]
  , prod = \case
    (Token Article a:Token RegexMatch (GroupMatch (noun:_)):Token Adjective adj:_) ->
      Just . Token SemanticNP $ semanticHelper_DT_NN_ADJ a noun adj
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ rule_NP__NU_NN
  , rule_NP__DT_NN
  , rule_NP__NP_PP
  , rule_NP__DT_ADJ_NN
  , rule_NP__DT_NN_ADJ
  ]
