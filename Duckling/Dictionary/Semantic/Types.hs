-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.Dictionary.Semantic.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text
import Duckling.Dictionary.Article.Types (ArticleData(..))

data SemanticData = SemanticData
  { mArticle :: Maybe ArticleData
  , subject :: Maybe Text
  , verb :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve SemanticData where
  type ResolvedValue SemanticData = SemanticValue

  resolve _ SemanticData {mArticle = Just article
                            , subject = Just subject }
   = Just $ selectSemanticArticleWithSubject article subject

  resolve _ _ = Nothing

data SemanticStructure = SemanticStructure
    { vMArticle :: ArticleData
    , vSubject :: Text
    , vVerb :: Maybe Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SemanticStructure where
    toJSON (SemanticStructure article subject verb) = object $
      [ "article" .= article
      , "subject" .= subject
      , "verb" .= Just verb
      ]

data SemanticValue
  = ExportSemanticValue SemanticStructure
  deriving (Eq, Ord, Show)

instance ToJSON SemanticValue where
  toJSON (ExportSemanticValue value) = case toJSON value of
    Object o -> Object $ o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

selectSemanticArticleWithSubject :: ArticleData -> Text -> SemanticValue
selectSemanticArticleWithSubject a s = ExportSemanticValue $ getSemanticArticleWithSubject a s

-- -----------------------------------------------------------------
-- Value build

getSemanticArticleWithSubject :: ArticleData -> Text -> SemanticStructure
getSemanticArticleWithSubject a s = SemanticStructure { vMArticle = a
                                    , vSubject = s
                                    , vVerb = Nothing}
