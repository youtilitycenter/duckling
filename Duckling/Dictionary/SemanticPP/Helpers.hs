-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}

module Duckling.Dictionary.SemanticPP.Helpers
  ( semanticHelper_PP__IN_NP
  , semanticHelper_PP__IN_DT_NN
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Article.Types (ArticleData(..))
import Duckling.Dictionary.Verb.Types (VerbData(..))
import Duckling.Dictionary.SemanticPP.Types (SemanticDataPP(..))
import Duckling.Dictionary.Preposition.Types (PrepositionData(..))
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Types
import qualified Duckling.Dictionary.SemanticPP.Types as TSemantic

-- -----------------------------------------------------------------
-- Production

semanticHelper_PP__IN_NP :: PrepositionData -> ArticleData -> Text -> SemanticDataPP
semanticHelper_PP__IN_NP p dt nn = SemanticDataPP { TSemantic.mIn = Just p
                                        , TSemantic.dt = Just dt
                                        , TSemantic.nn = Just nn
                                        , TSemantic.mNumber = Nothing }

semanticHelper_PP__IN_DT_NN :: PrepositionData -> NumeralData -> Text -> SemanticDataPP
semanticHelper_PP__IN_DT_NN p n nn = SemanticDataPP { TSemantic.mIn = Just p
                                        , TSemantic.dt = Nothing
                                        , TSemantic.nn = Just nn
                                        , TSemantic.mNumber = Just n }
