-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}

module Duckling.Dictionary.Semantic.Helpers
  ( semanticHelper__NP_VP
  , semanticHelper__NP
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Article.Types (ArticleData(..))
import Duckling.Dictionary.Semantic.Types (SemanticData(..))
import Duckling.Dictionary.SemanticNP.Types (SemanticDataNP(..))
import Duckling.Dictionary.SemanticVP.Types (SemanticDataVP(..))
import Duckling.Types
import qualified Duckling.Dictionary.Semantic.Types as TSemantic

-- -----------------------------------------------------------------
-- Production

semanticHelper__NP_VP :: SemanticDataNP -> SemanticDataVP -> SemanticData
semanticHelper__NP_VP np vp = SemanticData { TSemantic.mSemanticNp = Just np
                                    , TSemantic.mSemanticVp = Just vp }

semanticHelper__NP :: SemanticDataNP -> SemanticData
semanticHelper__NP np = SemanticData { TSemantic.mSemanticNp = Just np
                                    , TSemantic.mSemanticVp = Nothing }
