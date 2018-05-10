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
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SemanticDataNP where
    toJSON (SemanticDataNP mDt mNum mNn mNp mPp mAdj) = object $
      [ "article" .= mDt
      , "number" .= mNum
      , "noun" .= mNn
      , "noun_phrase" .= mNp
      , "pp" .= mPp
      , "adjective" .= mAdj
      ]

-- -----------------------------------------------------------------
-- resolve Data

instance Resolve SemanticDataNP where
  type ResolvedValue SemanticDataNP = SemanticValue

  resolve _ _ SemanticDataNP { mDt = Just mDt
                           , mNn = Just mNn
                           , mAdj = Just mAdj }
   = Just (selectSemantic_DT_NN_ADJ mDt mNn mAdj, False)

  resolve _ _ SemanticDataNP { mDt = Just mDt
                           , mNn = Just mNn }
   = Just (selectSemantic_DT_NN mDt mNn, False)

  resolve _ _ SemanticDataNP { mNum = Just mNum
                           , mNn = Just mNn }
   = Just (selectSemantic_NU_NN mNum mNn, False)

  resolve _ _ SemanticDataNP { mNp = Just mNp
                           , mPp = Just mPp }
   = Just (selectSemantic_NP_PP mNp mPp, False)

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
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

-- -----------------------------------------------------------------
-- toJSON structure

instance ToJSON SemanticStructureNP where
    toJSON (SemanticStructureNP mDt mNum mNn mNp mPp mAdj) = object $
      [ "article" .= mDt
      , "number" .= mNum
      , "noun" .= mNn
      , "noun_phrase" .= mNp
      , "pp" .= mPp
      , "adjective" .= mAdj
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

selectSemantic_DT_NN :: ArticleData -> Text -> SemanticValue
selectSemantic_DT_NN mDt mNn = ExportSemanticValueNP $ getSemantic_DT_NN mDt mNn

selectSemantic_NU_NN :: NumeralData -> Text -> SemanticValue
selectSemantic_NU_NN mNum mNn = ExportSemanticValueNP $ getSemantic_NU_NN mNum mNn

selectSemantic_NP_PP :: SemanticDataNP -> SemanticDataPP -> SemanticValue
selectSemantic_NP_PP np pp = ExportSemanticValueNP $ getSemantic_NP_PP np pp

selectSemantic_DT_NN_ADJ :: ArticleData -> Text -> AdjectiveData -> SemanticValue
selectSemantic_DT_NN_ADJ mDt mNn adj = ExportSemanticValueNP $ getSemantic_DT_NN_ADJ mDt mNn adj

-- -----------------------------------------------------------------
-- Value build

getSemantic_DT_NN :: ArticleData -> Text -> SemanticStructureNP
getSemantic_DT_NN mDt mNn = SemanticStructureNP { vMDt = Just mDt
                                              , vMNn = Just mNn
                                              , vMNum = Nothing
                                              , vMNp = Nothing
                                              , vMPp = Nothing
                                              , vMAdj = Nothing }

getSemantic_NU_NN :: NumeralData -> Text -> SemanticStructureNP
getSemantic_NU_NN mNum mNn = SemanticStructureNP { vMDt = Nothing
                                              , vMNn = Just mNn
                                              , vMNum = Just mNum
                                              , vMNp = Nothing
                                              , vMPp = Nothing
                                              , vMAdj = Nothing }

getSemantic_NP_PP :: SemanticDataNP -> SemanticDataPP -> SemanticStructureNP
getSemantic_NP_PP np pp = SemanticStructureNP { vMNp = Just np
                                              , vMPp = Just pp
                                              , vMNum = Nothing
                                              , vMNn = Nothing
                                              , vMDt = Nothing
                                              , vMAdj = Nothing }

getSemantic_DT_NN_ADJ :: ArticleData -> Text -> AdjectiveData -> SemanticStructureNP
getSemantic_DT_NN_ADJ mDt mNn mAdj = SemanticStructureNP { vMDt = Just mDt
                                              , vMNn = Just mNn
                                              , vMNum = Nothing
                                              , vMNp = Nothing
                                              , vMPp = Nothing
                                              , vMAdj = Just mAdj }
