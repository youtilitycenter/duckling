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
  , noun :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SemanticDataVP where
    toJSON (SemanticDataVP mVerb np pp vp adv noun) = object $
      [ "verb" .= mVerb
      , "noun_phrase" .= np
      , "pp" .= pp
      , "verb_phrase" .= vp
      , "adverb" .= adv
      , "noun" .= noun
      ]

-- -----------------------------------------------------------------
-- resolve Data

instance Resolve SemanticDataVP where
  type ResolvedValue SemanticDataVP = SemanticValue

  resolve _ SemanticDataVP { mVerb = Just mVerb
                           , np = Just np }
   = Just $ selectSemantic_Vt_NP mVerb np

  resolve _ SemanticDataVP { vp = Just vp
                           , noun = Just noun }
   = Just $ selectSemantic_NN_VP noun vp

  resolve _ SemanticDataVP { mVerb = Just mVerb
                           , adv = Just adv }
   = Just $ selectSemantic_V_ADV mVerb adv

  resolve _ SemanticDataVP { vp = Just vp
                           , pp = Just pp }
   = Just $ selectSemantic_VP_PP vp pp

  resolve _ SemanticDataVP { mVerb = Just mVerb }
   = Just $ selectSemantic_Vi mVerb

  resolve _ _ = Nothing

-- -----------------------------------------------------------------
-- structure

data SemanticStructureVP = SemanticStructureVP
    { vMVerb :: Maybe VerbData
    , vNP :: Maybe SemanticDataNP
    , vPP :: Maybe SemanticDataPP
    , vVP :: Maybe SemanticDataVP
    , vAdv :: Maybe AdverbData
    , vNoun :: Maybe Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

-- -----------------------------------------------------------------
-- toJSON structure

instance ToJSON SemanticStructureVP where
    toJSON (SemanticStructureVP mVerb np pp vp adv noun) = object $
      [ "verb" .= mVerb
      , "noun_phrase" .= np
      , "pp" .= pp
      , "verb_phrase" .= vp
      , "adverb" .= adv
      , "noun" .= noun
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

selectSemantic_Vt_NP :: VerbData -> SemanticDataNP -> SemanticValue
selectSemantic_Vt_NP v np = ExportSemanticValueVP $ getSemantic_Vt_NP v np

selectSemantic_NN_VP :: Text -> SemanticDataVP -> SemanticValue
selectSemantic_NN_VP noun vp = ExportSemanticValueVP $ getSemantic_NN_VP noun vp

selectSemantic_VP_PP :: SemanticDataVP -> SemanticDataPP -> SemanticValue
selectSemantic_VP_PP vp pp = ExportSemanticValueVP $ getSemantic_VP_PP vp pp

selectSemantic_Vi :: VerbData -> SemanticValue
selectSemantic_Vi v = ExportSemanticValueVP $ getSemantic_Vi v

selectSemantic_V_ADV :: VerbData -> AdverbData -> SemanticValue
selectSemantic_V_ADV v adv = ExportSemanticValueVP $ getSemantic_V_ADV v adv

-- -----------------------------------------------------------------
-- Value build

getSemantic_Vt_NP :: VerbData -> SemanticDataNP -> SemanticStructureVP
getSemantic_Vt_NP v np = SemanticStructureVP { vMVerb = Just v
                                       , vNP = Just np
                                       , vPP = Nothing
                                       , vVP = Nothing
                                       , vAdv = Nothing
                                       , vNoun = Nothing }

getSemantic_NN_VP :: Text -> SemanticDataVP -> SemanticStructureVP
getSemantic_NN_VP noun vp = SemanticStructureVP { vMVerb = Nothing
                                      , vNP = Nothing
                                      , vPP = Nothing
                                      , vVP = Just vp
                                      , vAdv = Nothing
                                      , vNoun = Just noun }

getSemantic_VP_PP :: SemanticDataVP -> SemanticDataPP -> SemanticStructureVP
getSemantic_VP_PP vp pp = SemanticStructureVP { vMVerb = Nothing
                                      , vNP = Nothing
                                      , vPP = Just pp
                                      , vVP = Just vp
                                      , vAdv = Nothing
                                      , vNoun = Nothing  }

getSemantic_Vi :: VerbData -> SemanticStructureVP
getSemantic_Vi v = SemanticStructureVP { vMVerb = Just v
                                       , vNP = Nothing
                                       , vPP = Nothing
                                       , vVP = Nothing
                                       , vAdv = Nothing
                                       , vNoun = Nothing  }

getSemantic_V_ADV :: VerbData -> AdverbData -> SemanticStructureVP
getSemantic_V_ADV v adv = SemanticStructureVP { vMVerb = Just v
                                      , vNP = Nothing
                                      , vPP = Nothing
                                      , vVP = Nothing
                                      , vAdv = Just adv
                                      , vNoun = Nothing  }
