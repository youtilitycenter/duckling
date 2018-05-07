-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}

module Duckling.Dictionary.SemanticNP.Helpers
  ( semanticHelper_NP_PP
  , semanticHelper_NU_NN
  , semanticHelper_DT_NN
  , semanticHelper_DT_NN_ADJ
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Article.Types (ArticleData(..))
import Duckling.Dictionary.Adjective.Types (AdjectiveData(..))
import Duckling.Dictionary.SemanticNP.Types (SemanticDataNP(..))
import Duckling.Dictionary.SemanticPP.Types (SemanticDataPP(..))
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Types
import qualified Duckling.Dictionary.SemanticNP.Types as TSemantic

-- -----------------------------------------------------------------
-- Production

semanticHelper_DT_NN :: ArticleData -> Text -> SemanticDataNP
semanticHelper_DT_NN dt nn = SemanticDataNP { TSemantic.mDt = Just dt
                                      , TSemantic.mNn = Just nn
                                      , TSemantic.mNum = Nothing
                                      , TSemantic.mNp = Nothing
                                      , TSemantic.mPp = Nothing
                                      , TSemantic.mAdj = Nothing }

semanticHelper_NU_NN :: NumeralData -> Text -> SemanticDataNP
semanticHelper_NU_NN num nn = SemanticDataNP { TSemantic.mDt = Nothing
                                      , TSemantic.mNn = Just nn
                                      , TSemantic.mNum = Just num
                                      , TSemantic.mNp = Nothing
                                      , TSemantic.mPp = Nothing
                                      , TSemantic.mAdj = Nothing }

semanticHelper_NP_PP :: SemanticDataNP -> SemanticDataPP -> SemanticDataNP
semanticHelper_NP_PP np pp = SemanticDataNP { TSemantic.mDt = Nothing
                                            , TSemantic.mNn = Nothing
                                            , TSemantic.mNum = Nothing
                                            , TSemantic.mNp = Just np
                                            , TSemantic.mPp = Just pp
                                            , TSemantic.mAdj = Nothing }

semanticHelper_DT_NN_ADJ :: ArticleData -> Text -> AdjectiveData -> SemanticDataNP
semanticHelper_DT_NN_ADJ dt nn adj = SemanticDataNP { TSemantic.mDt = Just dt
                                      , TSemantic.mNn = Just nn
                                      , TSemantic.mNum = Nothing
                                      , TSemantic.mNp = Nothing
                                      , TSemantic.mPp = Nothing
                                      , TSemantic.mAdj = Just adj }
