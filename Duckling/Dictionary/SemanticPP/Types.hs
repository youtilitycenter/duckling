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

module Duckling.Dictionary.SemanticPP.Types where

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
import Duckling.Dictionary.Verb.Types (VerbData(..))
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Dictionary.Preposition.Types (PrepositionData(..))

-- -----------------------------------------------------------------
-- define Data

data SemanticDataPP = SemanticDataPP
  { mIn :: Maybe PrepositionData
  , dt :: Maybe ArticleData
  , mNumber :: Maybe NumeralData
  , nn :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SemanticDataPP where
    toJSON (SemanticDataPP mIn dt mNumber nn) = object $
      [ "preposition" .= mIn
      , "article" .= dt
      , "number" .= mNumber
      , "noun" .= nn
      ]

-- -----------------------------------------------------------------
-- resolve Data

instance Resolve SemanticDataPP where
  type ResolvedValue SemanticDataPP = SemanticValue

  resolve _ SemanticDataPP { mIn = Just mIn
                           , dt = Just dt
                           , nn = Just nn }
   = Just $ selectSemantic_PP__IN_NP mIn dt nn

  resolve _ SemanticDataPP { mIn = Just mIn
                           , mNumber = Just mNumber
                           , nn = Just nn }
   = Just $ selectSemantic_PP__IN_DT_NN mIn mNumber nn

  resolve _ _ = Nothing

-- -----------------------------------------------------------------
-- structure

data SemanticStructurePP = SemanticStructurePP
    { vMIn :: Maybe PrepositionData
    , vDt :: Maybe ArticleData
    , vMNumber :: Maybe NumeralData
    , vNn :: Maybe Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

-- -----------------------------------------------------------------
-- toJSON structure

instance ToJSON SemanticStructurePP where
    toJSON (SemanticStructurePP mIn dt mNumber nn) = object $
      [ "preposition" .= mIn
      , "article" .= dt
      , "number" .= mNumber
      , "noun" .= nn
      ]

-- -----------------------------------------------------------------
-- Value

data SemanticValue
  = ExportSemanticValuePP SemanticStructurePP
  deriving (Eq, Ord, Show)

instance ToJSON SemanticValue where
  toJSON (ExportSemanticValuePP value) = case toJSON value of
    Object o -> Object $ o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

selectSemantic_PP__IN_NP :: PrepositionData -> ArticleData -> Text -> SemanticValue
selectSemantic_PP__IN_NP p dt nn = ExportSemanticValuePP $ getSemantic_PP__IN_NP p dt nn

selectSemantic_PP__IN_DT_NN :: PrepositionData -> NumeralData -> Text -> SemanticValue
selectSemantic_PP__IN_DT_NN p dt nn = ExportSemanticValuePP $ getSemantic_PP__IN_DT_NN p dt nn

-- -----------------------------------------------------------------
-- Value build

getSemantic_PP__IN_NP :: PrepositionData -> ArticleData -> Text -> SemanticStructurePP
getSemantic_PP__IN_NP p dt nn = SemanticStructurePP { vMIn = Just p
                                          , vDt = Just dt
                                          , vNn = Just nn
                                          , vMNumber = Nothing }

getSemantic_PP__IN_DT_NN :: PrepositionData -> NumeralData -> Text -> SemanticStructurePP
getSemantic_PP__IN_DT_NN p n nn = SemanticStructurePP { vMIn = Just p
                                          , vDt = Nothing
                                          , vNn = Just nn
                                          , vMNumber = Just n }
