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

module Duckling.Dictionary.SemanticVP.Types where

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
import Duckling.Dictionary.Adverb.Types (AdverbData(..))
import Duckling.Dictionary.Verb.Types (VerbData(..))
import Duckling.Dictionary.SemanticNP.Types (SemanticDataNP(..))
import Duckling.Dictionary.SemanticPP.Types (SemanticDataPP(..))

-- -----------------------------------------------------------------
-- define Data

data SemanticDataVP = SemanticDataVP
  { mVerb :: Maybe VerbData
  , np :: Maybe SemanticDataNP
  , pp :: Maybe SemanticDataPP
  , vp :: Maybe SemanticDataVP
  , adv :: Maybe AdverbData
  , vpTag :: Maybe Text
  , vpRefTag :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SemanticDataVP where
    toJSON (SemanticDataVP mVerb np pp vp adv vpTag vpRefTag) = object $
      [ "verb" .= mVerb
      , "noun_phrase" .= np
      , "pp" .= pp
      , "verb_phrase" .= vp
      , "adverb" .= adv
      , "tag" .= vpTag
      , "ref_tag" .= vpRefTag
      ]

-- -----------------------------------------------------------------
-- resolve Data

instance Resolve SemanticDataVP where
  type ResolvedValue SemanticDataVP = SemanticValue

  resolve _ _ SemanticDataVP { mVerb = Just mVerb
                            , np = Just np
                            , vpTag = Just vpTag
                            , vpRefTag = Just vpRefTag }
   = Just (selectSemantic_Vt_NP mVerb np vpTag vpRefTag, False)

  resolve _ _ SemanticDataVP { vp = Just vp
                           , np = Just np
                           , vpTag = Just vpTag
                           , vpRefTag = Just vpRefTag }
   = Just (selectSemantic_NN_VP np vp vpTag vpRefTag, False)

  resolve _ _ SemanticDataVP { mVerb = Just mVerb
                           , adv = Just adv
                           , vpTag = Just vpTag
                           , vpRefTag = Just vpRefTag }
   = Just (selectSemantic_V_ADV mVerb adv vpTag vpRefTag, False)

  resolve _ _ SemanticDataVP { vp = Just vp
                           , pp = Just pp
                           , vpTag = Just vpTag
                           , vpRefTag = Just vpRefTag }
   = Just (selectSemantic_VP_PP vp pp vpTag vpRefTag, False)

  resolve _ _ SemanticDataVP { mVerb = Just mVerb
                           , vpTag = Just vpTag
                           , vpRefTag = Just vpRefTag }
   = Just (selectSemantic_Vi mVerb vpTag vpRefTag, False)

  resolve _ _ _ = Nothing

-- -----------------------------------------------------------------
-- structure

data SemanticStructureVP = SemanticStructureVP
    { vMVerb :: Maybe VerbData
    , vNP :: Maybe SemanticDataNP
    , vPP :: Maybe SemanticDataPP
    , vVP :: Maybe SemanticDataVP
    , vAdv :: Maybe AdverbData
    , vVpTag :: Maybe Text
    , vVpRefTag :: Maybe Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

-- -----------------------------------------------------------------
-- toJSON structure

instance ToJSON SemanticStructureVP where
    toJSON (SemanticStructureVP mVerb np pp vp adv vpTag vpRefTag) = object $
      [ "verb" .= mVerb
      , "noun_phrase" .= np
      , "pp" .= pp
      , "verb_phrase" .= vp
      , "adverb" .= adv
      , "tag" .= vpTag
      , "ref_tag" .= vpRefTag
      ]

-- -----------------------------------------------------------------
-- Value

data SemanticValue
  = ExportSemanticValueVP SemanticStructureVP
  deriving (Eq, Ord, Show)

instance ToJSON SemanticValue where
  toJSON (ExportSemanticValueVP value) = case toJSON value of
    Object o -> Object $ o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

selectSemantic_Vt_NP :: VerbData -> SemanticDataNP -> Text -> Text -> SemanticValue
selectSemantic_Vt_NP v np vpTag vpRefTag = ExportSemanticValueVP $ getSemantic_Vt_NP v np vpTag vpRefTag

selectSemantic_NN_VP :: SemanticDataNP -> SemanticDataVP -> Text -> Text -> SemanticValue
selectSemantic_NN_VP np vp vpTag vpRefTag = ExportSemanticValueVP $ getSemantic_NN_VP np vp vpTag vpRefTag

selectSemantic_VP_PP :: SemanticDataVP -> SemanticDataPP -> Text -> Text -> SemanticValue
selectSemantic_VP_PP vp pp vpTag vpRefTag = ExportSemanticValueVP $ getSemantic_VP_PP vp pp vpTag vpRefTag

selectSemantic_Vi :: VerbData -> Text -> Text -> SemanticValue
selectSemantic_Vi v vpTag vpRefTag = ExportSemanticValueVP $ getSemantic_Vi v vpTag vpRefTag

selectSemantic_V_ADV :: VerbData -> AdverbData -> Text -> Text -> SemanticValue
selectSemantic_V_ADV v adv vpTag vpRefTag = ExportSemanticValueVP $ getSemantic_V_ADV v adv vpTag vpRefTag

-- -----------------------------------------------------------------
-- Value build

getSemantic_Vt_NP :: VerbData -> SemanticDataNP -> Text -> Text -> SemanticStructureVP
getSemantic_Vt_NP v np vpTag vpRefTag = SemanticStructureVP { vMVerb = Just v
                                       , vNP = Just np
                                       , vPP = Nothing
                                       , vVP = Nothing
                                       , vAdv = Nothing
                                       , vVpTag = Just vpTag
                                       , vVpRefTag = Just vpRefTag }

getSemantic_NN_VP :: SemanticDataNP -> SemanticDataVP -> Text -> Text -> SemanticStructureVP
getSemantic_NN_VP np vp vpTag vpRefTag = SemanticStructureVP { vMVerb = Nothing
                                      , vNP = Just np
                                      , vPP = Nothing
                                      , vVP = Just vp
                                      , vAdv = Nothing
                                      , vVpTag = Just vpTag
                                      , vVpRefTag = Just vpRefTag }

getSemantic_VP_PP :: SemanticDataVP -> SemanticDataPP -> Text -> Text -> SemanticStructureVP
getSemantic_VP_PP vp pp vpTag vpRefTag = SemanticStructureVP { vMVerb = Nothing
                                      , vNP = Nothing
                                      , vPP = Just pp
                                      , vVP = Just vp
                                      , vAdv = Nothing
                                      , vVpTag = Just vpTag
                                      , vVpRefTag = Just vpRefTag  }

getSemantic_Vi :: VerbData -> Text -> Text -> SemanticStructureVP
getSemantic_Vi v vpTag vpRefTag = SemanticStructureVP { vMVerb = Just v
                                       , vNP = Nothing
                                       , vPP = Nothing
                                       , vVP = Nothing
                                       , vAdv = Nothing
                                       , vVpTag = Just vpTag
                                       , vVpRefTag = Just vpRefTag  }

getSemantic_V_ADV :: VerbData -> AdverbData -> Text -> Text -> SemanticStructureVP
getSemantic_V_ADV v adv vpTag vpRefTag = SemanticStructureVP { vMVerb = Just v
                                      , vNP = Nothing
                                      , vPP = Nothing
                                      , vVP = Nothing
                                      , vAdv = Just adv
                                      , vVpTag = Just vpTag
                                      , vVpRefTag = Just vpRefTag  }
