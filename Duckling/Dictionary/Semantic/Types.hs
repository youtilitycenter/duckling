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
import Duckling.Dictionary.SemanticNP.Types (SemanticDataNP(..))
import Duckling.Dictionary.SemanticVP.Types (SemanticDataVP(..))
import Duckling.Dictionary.Verb.Types (VerbData(..))

-- -----------------------------------------------------------------
-- define Data

data SemanticData = SemanticData
  { mSemanticNp :: Maybe SemanticDataNP
  , mSemanticVp :: Maybe SemanticDataVP
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

-- -----------------------------------------------------------------
-- resolve Data

instance Resolve SemanticData where
  type ResolvedValue SemanticData = SemanticValue

  resolve _ SemanticData { mSemanticNp = Just mSemanticNp
                         , mSemanticVp = Just mSemanticVp }
   = Just $ selectSemantic mSemanticNp mSemanticVp

  resolve _ _ = Nothing

-- -----------------------------------------------------------------
-- structure

data SemanticStructure = SemanticStructure
    { vMSemanticNp :: Maybe SemanticDataNP
    , vMSemanticVp :: Maybe SemanticDataVP
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

-- -----------------------------------------------------------------
-- toJSON structure

instance ToJSON SemanticStructure where
    toJSON (SemanticStructure mSemanticNp mSemanticVp) = object $
      [ "noun_phrase" .= mSemanticNp
      , "verb_phrase" .= mSemanticVp
      ]

-- -----------------------------------------------------------------
-- Value

data SemanticValue
  = ExportSemanticValue SemanticStructure
  deriving (Eq, Ord, Show)

instance ToJSON SemanticValue where
  toJSON (ExportSemanticValue value) = case toJSON value of
    Object o -> Object $ o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

selectSemantic :: SemanticDataNP -> SemanticDataVP -> SemanticValue
selectSemantic np vp = ExportSemanticValue $ getSemantic np vp

-- -----------------------------------------------------------------
-- Value build

getSemantic :: SemanticDataNP -> SemanticDataVP -> SemanticStructure
getSemantic np vp = SemanticStructure { vMSemanticNp = Just np
                                      , vMSemanticVp = Just vp }
