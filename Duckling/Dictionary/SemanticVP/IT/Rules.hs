-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.SemanticVP.IT.Rules
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
import Duckling.Dictionary.SemanticVP.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Dictionary.Adverb.IT.Rules (searchAdverb, adverbs)
import qualified Duckling.Numeral.Types as TNumeral


rule_VP__Vt_NP :: Rule
rule_VP__Vt_NP = Rule
  { name = "<verb> <np>"
  , pattern =
    [ dimension Verb
    , dimension SemanticNP
    ]
  , prod = \case
    (Token Verb v:Token SemanticNP np:_) ->
      Just . Token SemanticVP $ semanticHelper_Vt_NP v np
    _ -> Nothing
  }

rule_VP__NN_VP :: Rule
rule_VP__NN_VP = Rule
  { name = "noun <vp>"
  , pattern =
    [ regex "(\\w+)"
    , dimension SemanticVP
    ]
  , prod = \case
    (Token RegexMatch (GroupMatch (noun:_)):Token SemanticVP vp:_) ->
      if null (searchAdverb noun adverbs) == False 
        then Nothing
        else Just . Token SemanticVP $ semanticHelper_NN_VP noun vp
    _ -> Nothing
  }

rule_VP__VP_NN :: Rule
rule_VP__VP_NN = Rule
  { name = "<vp> noun"
  , pattern =
    [ dimension SemanticVP
    , regex "(\\w+)"
    ]
  , prod = \case
    (Token SemanticVP vp:Token RegexMatch (GroupMatch (noun:_)):_) ->
      if null (searchAdverb noun adverbs) == False 
        then Nothing
        else Just . Token SemanticVP $ semanticHelper_NN_VP noun vp
    _ -> Nothing
  }

rule_VP__VP_PP :: Rule
rule_VP__VP_PP = Rule
  { name = "<vp> <pp>"
  , pattern =
    [ dimension SemanticVP
    , dimension SemanticPP
    ]
  , prod = \case
    (Token SemanticVP vp:Token SemanticPP pp:_) ->
      Just . Token SemanticVP $ semanticHelper_VP_PP vp pp
    _ -> Nothing
  }

rule_VP__Vi :: Rule
rule_VP__Vi = Rule
  { name = "<verb>"
  , pattern =
    [ dimension Verb
    ]
  , prod = \case
    (Token Verb v:_) ->
      Just . Token SemanticVP $ semanticHelper_Vi v
    _ -> Nothing
  }

rule_VP__V_ADV :: Rule
rule_VP__V_ADV = Rule
  { name = "<verb> <adverb>"
  , pattern =
    [ dimension Verb
    , dimension Adverb
    ]
  , prod = \case
    (Token Verb v:Token Adverb adv:_) ->
      Just . Token SemanticVP $ semanticHelper_V_ADV v adv
    _ -> Nothing
  }

rule_VP__ADV_V :: Rule
rule_VP__ADV_V = Rule
  { name = "<adverb> <verb>"
  , pattern =
    [ dimension Adverb
    , dimension Verb
    ]
  , prod = \case
    (Token Adverb adv:Token Verb v:_) ->
      Just . Token SemanticVP $ semanticHelper_V_ADV v adv
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ rule_VP__Vt_NP
  , rule_VP__VP_PP
  , rule_VP__Vi
  , rule_VP__ADV_V
  , rule_VP__V_ADV
  , rule_VP__NN_VP
  , rule_VP__VP_NN
  ]
