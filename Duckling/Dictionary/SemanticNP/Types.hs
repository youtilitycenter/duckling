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

module Duckling.Dictionary.SemanticNP.Types where

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
import Duckling.Dictionary.Adjective.Types (AdjectiveData(..))
import Duckling.Dictionary.Verb.Types (VerbData(..))
import Duckling.Dictionary.SemanticPP.Types (SemanticDataPP(..))
import Duckling.Numeral.Types (NumeralData(..))

-- -----------------------------------------------------------------
-- define Data

data SemanticDataNP = SemanticDataNP
  { mDt :: Maybe ArticleData
  , mNum :: Maybe NumeralData
  , mNn :: Maybe Text
  , mNp :: Maybe SemanticDataNP
  , mPp :: Maybe SemanticDataPP
  , mAdj :: Maybe AdjectiveData
  , npTag :: Maybe Text
  , npRefTag :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SemanticDataNP where
    toJSON (SemanticDataNP mDt mNum mNn mNp mPp mAdj npTag npRefTag) = object $
      [ "article" .= mDt
      , "number" .= mNum
      , "noun" .= mNn
      , "noun_phrase" .= mNp
      , "pp" .= mPp
      , "adjective" .= mAdj
      , "tag" .= npTag
      , "ref_tag" .= npRefTag
      ]

-- -----------------------------------------------------------------
-- resolve Data

instance Resolve SemanticDataNP where
  type ResolvedValue SemanticDataNP = SemanticValue

  resolve _ _ SemanticDataNP { mDt = Just mDt
                           , mNn = Just mNn
                           , mAdj = Just mAdj
                           , vpTag = Just vpTag
                           , vpRefTag = Just vpRefTag }
   = Just (selectSemantic_DT_NN_ADJ mDt mNn mAdj vpTag vpRefTag, False)

  resolve _ _ SemanticDataNP { mDt = Just mDt
                           , mNn = Just mNn
                           , vpTag = Just vpTag
                           , vpRefTag = Just vpRefTag }
   = Just (selectSemantic_DT_NN mDt mNn vpTag vpRefTag, False)

  resolve _ _ SemanticDataNP { mNum = Just mNum
                           , mNn = Just mNn
                           , vpTag = Just vpTag
                           , vpRefTag = Just vpRefTag }
   = Just (selectSemantic_NU_NN mNum mNn vpTag vpRefTag, False)

  resolve _ _ SemanticDataNP { mNp = Just mNp
                           , mPp = Just mPp
                           , vpTag = Just vpTag
                           , vpRefTag = Just vpRefTag }
   = Just (selectSemantic_NP_PP mNp mPp vpTag vpRefTag, False)

  resolve _ _ _ = Nothing

-- -----------------------------------------------------------------
-- structure

data SemanticStructureNP = SemanticStructureNP
    { vMDt :: Maybe ArticleData
    , vMNum :: Maybe NumeralData
    , vMNn :: Maybe Text
    , vMNp :: Maybe SemanticDataNP
    , vMPp :: Maybe SemanticDataPP
    , vMAdj :: Maybe AdjectiveData
    , vVpTag :: Maybe Text
    , vVpRefTag :: Maybe Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

-- -----------------------------------------------------------------
-- toJSON structure

instance ToJSON SemanticStructureNP where
    toJSON (SemanticStructureNP mDt mNum mNn mNp mPp mAdj vpTag vpRefTag) = object $
      [ "article" .= mDt
      , "number" .= mNum
      , "noun" .= mNn
      , "noun_phrase" .= mNp
      , "pp" .= mPp
      , "adjective" .= mAdj
      , "tag" .= vpTag
      , "ref_tag" .= vpRefTag
      ]

-- -----------------------------------------------------------------
-- Value

data SemanticValue
  = ExportSemanticValueNP SemanticStructureNP
  deriving (Eq, Ord, Show)

instance ToJSON SemanticValue where
  toJSON (ExportSemanticValueNP value) = case toJSON value of
    Object o -> Object $ o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

selectSemantic_DT_NN :: ArticleData -> Text -> Text -> Text -> SemanticValue
selectSemantic_DT_NN mDt mNn vpTag vpRefTag = ExportSemanticValueNP $ getSemantic_DT_NN mDt mNn vpTag vpRefTag

selectSemantic_NU_NN :: NumeralData -> Text -> Text -> Text -> SemanticValue
selectSemantic_NU_NN mNum mNn vpTag vpRefTag = ExportSemanticValueNP $ getSemantic_NU_NN mNum mNn vpTag vpRefTag

selectSemantic_NP_PP :: SemanticDataNP -> SemanticDataPP -> Text -> Text -> SemanticValue
selectSemantic_NP_PP np pp vpTag vpRefTag = ExportSemanticValueNP $ getSemantic_NP_PP np pp vpTag vpRefTag

selectSemantic_DT_NN_ADJ :: ArticleData -> Text -> AdjectiveData -> Text -> Text -> SemanticValue
selectSemantic_DT_NN_ADJ mDt mNn adj vpTag vpRefTag = ExportSemanticValueNP $ getSemantic_DT_NN_ADJ mDt mNn adj vpTag vpRefTag

-- -----------------------------------------------------------------
-- Value build

getSemantic_DT_NN :: ArticleData -> Text -> Text -> Text -> SemanticStructureNP
getSemantic_DT_NN mDt mNn vpTag vpRefTag = SemanticStructureNP { vMDt = Just mDt
                                              , vMNn = Just mNn
                                              , vMNum = Nothing
                                              , vMNp = Nothing
                                              , vMPp = Nothing
                                              , vMAdj = Nothing
                                              , vVpTag = Just vpTag
                                              , vVpRefTag = Just vpRefTag }

getSemantic_NU_NN :: NumeralData -> Text -> Text -> Text -> SemanticStructureNP
getSemantic_NU_NN mNum mNn vpTag vpRefTag = SemanticStructureNP { vMDt = Nothing
                                              , vMNn = Just mNn
                                              , vMNum = Just mNum
                                              , vMNp = Nothing
                                              , vMPp = Nothing
                                              , vMAdj = Nothing
                                              , vVpTag = Just vpTag
                                              , vVpRefTag = Just vpRefTag }

getSemantic_NP_PP :: SemanticDataNP -> SemanticDataPP -> Text -> Text -> SemanticStructureNP
getSemantic_NP_PP np pp vpTag vpRefTag = SemanticStructureNP { vMNp = Just np
                                              , vMPp = Just pp
                                              , vMNum = Nothing
                                              , vMNn = Nothing
                                              , vMDt = Nothing
                                              , vMAdj = Nothing
                                              , vVpTag = Just vpTag
                                              , vVpRefTag = Just vpRefTag }

getSemantic_DT_NN_ADJ :: ArticleData -> Text -> AdjectiveData -> Text -> Text -> SemanticStructureNP
getSemantic_DT_NN_ADJ mDt mNn mAdj vpTag vpRefTag = SemanticStructureNP { vMDt = Just mDt
                                              , vMNn = Just mNn
                                              , vMNum = Nothing
                                              , vMNp = Nothing
                                              , vMPp = Nothing
                                              , vMAdj = Just mAdj
                                              , vVpTag = Just vpTag
                                              , vVpRefTag = Just vpRefTag }
