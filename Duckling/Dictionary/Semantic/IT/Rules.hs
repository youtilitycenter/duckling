-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.Semantic.IT.Rules
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
import Duckling.Dictionary.Semantic.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Dictionary.Semantic.Types (SemanticData(..))
import qualified Duckling.Numeral.Types as TNumeral

rule_NP :: Rule
rule_NP = Rule
  { name = "<np>"
  , pattern =
    [ dimension SemanticNP
    ]
  , prod = \case
    (Token SemanticNP np:_) ->
      Just . Token Semantic $ semanticHelper__NP np
    _ -> Nothing
  }

rule_NP_VP :: Rule
rule_NP_VP = Rule
  { name = "<np> <vp>"
  , pattern =
    [ dimension SemanticNP
    , dimension SemanticVP
    ]
  , prod = \case
    (Token SemanticNP np:Token SemanticVP vp:_) ->
      Just . Token Semantic $ semanticHelper__NP_VP np vp
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ rule_NP
  , rule_NP_VP
  ]
