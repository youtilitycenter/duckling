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

module Duckling.Dictionary.Article.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

data ArticleData = ArticleData
  { article :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON ArticleData where
    toJSON (ArticleData article) = object $
      ["value" .= article
      ]

instance Resolve ArticleData where
  type ResolvedValue ArticleData = ArticleValue

  resolve _ _ ArticleData {article = Just article}
   = Just (simple article, False)

data SingleValue = SingleValue
    { vArticle :: Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SingleValue where
    toJSON (SingleValue article) = object $
      ["article" .= article
      ]

data ArticleValue
  = SimpleValue SingleValue
  deriving (Eq, Ord, Show)

instance ToJSON ArticleValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

simple :: Text -> ArticleValue
simple a = SimpleValue $ single a

single :: Text -> SingleValue
single a = SingleValue {vArticle = a}
