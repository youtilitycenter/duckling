-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}

module Duckling.Dictionary.SemanticVP.Helpers
  ( semanticHelper_Vt_NP
  , semanticHelper_VP_PP
  , semanticHelper_Vi
  , semanticHelper_V_ADV
  , semanticHelper_NN_VP
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Article.Types (ArticleData(..))
import Duckling.Dictionary.Verb.Types (VerbData(..))
import Duckling.Dictionary.SemanticVP.Types (SemanticDataVP(..))
import Duckling.Dictionary.SemanticNP.Types (SemanticDataNP(..))
import Duckling.Dictionary.SemanticPP.Types (SemanticDataPP(..))
import Duckling.Dictionary.Adverb.Types (AdverbData(..))
import Duckling.Types
import qualified Duckling.Dictionary.SemanticVP.Types as TSemantic

-- -----------------------------------------------------------------
-- Production

semanticHelper_Vt_NP :: VerbData -> SemanticDataNP -> SemanticDataVP
semanticHelper_Vt_NP v np = SemanticDataVP { TSemantic.mVerb = Just v
                                  , TSemantic.np = Just np
                                  , TSemantic.pp = Nothing
                                  , TSemantic.vp = Nothing
                                  , TSemantic.adv = Nothing
                                  , TSemantic.noun = Nothing }

semanticHelper_NN_VP :: Text -> SemanticDataVP -> SemanticDataVP
semanticHelper_NN_VP noun vp = SemanticDataVP { TSemantic.mVerb = Nothing
                                  , TSemantic.np = Nothing
                                  , TSemantic.pp = Nothing
                                  , TSemantic.vp = Just vp
                                  , TSemantic.adv = Nothing
                                  , TSemantic.noun = Just noun }

semanticHelper_VP_PP :: SemanticDataVP -> SemanticDataPP -> SemanticDataVP
semanticHelper_VP_PP vp pp = SemanticDataVP { TSemantic.mVerb = Nothing
                                  , TSemantic.np = Nothing
                                  , TSemantic.pp = Just pp
                                  , TSemantic.vp = Just vp
                                  , TSemantic.adv = Nothing
                                  , TSemantic.noun = Nothing }

semanticHelper_Vi :: VerbData -> SemanticDataVP
semanticHelper_Vi v = SemanticDataVP { TSemantic.mVerb = Just v
                                  , TSemantic.np = Nothing
                                  , TSemantic.pp = Nothing
                                  , TSemantic.vp = Nothing
                                  , TSemantic.adv = Nothing
                                  , TSemantic.noun = Nothing }

semanticHelper_V_ADV :: VerbData -> AdverbData -> SemanticDataVP
semanticHelper_V_ADV v adv = SemanticDataVP { TSemantic.mVerb = Just v
                                  , TSemantic.np = Nothing
                                  , TSemantic.pp = Nothing
                                  , TSemantic.vp = Nothing
                                  , TSemantic.adv = Just adv
                                  , TSemantic.noun = Nothing }
