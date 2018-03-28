-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}

module Duckling.Dictionary.Semantic.Helpers
  ( semanticHelper
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Article.Types (ArticleData(..))
import Duckling.Dictionary.Semantic.Types (SemanticData(..))
import Duckling.Types
import qualified Duckling.Dictionary.Semantic.Types as TSemantic

-- -----------------------------------------------------------------
-- Production

semanticHelper :: ArticleData -> Text -> Text -> SemanticData
semanticHelper a s v = SemanticData {TSemantic.mArticle = Just a
                                  , TSemantic.subject = Just s
                                  , TSemantic.verb = Just v}
