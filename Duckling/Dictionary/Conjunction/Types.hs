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

module Duckling.Dictionary.Conjunction.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

data ConjunctionData = ConjunctionData
  { conjunction :: Text
  , mtype :: Text
  , adjective :: Maybe Text
  , group :: Text
  , keyword :: String
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve ConjunctionData where
  type ResolvedValue ConjunctionData = ConjunctionValue

  resolve _ ConjunctionData { group = group
                            , keyword = keyword
                            , conjunction = conjunction
                            , mtype = mtype
                            , adjective = Just adjective}
   = Just $ selectConjunction group keyword conjunction mtype adjective

  resolve _ _ = Nothing

data ConjunctionStructure = ConjunctionStructure
    { vConjunction :: Text
    , vMType :: Text
    , vAdjective :: Text
    , vGroup :: Text
    , vKeyword :: String
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON ConjunctionStructure where
    toJSON (ConjunctionStructure conjunction mtype adjective group keyword) = object $
      [ "group" .= group
      , "keyword" .= keyword
      , "conjunction" .= conjunction
      , "type" .= mtype
      , "adjective" .= adjective
      ]

data ConjunctionValue
  = ExportConjunctionValue ConjunctionStructure
  deriving (Eq, Ord, Show)

instance ToJSON ConjunctionValue where
  toJSON (ExportConjunctionValue value) = case toJSON value of
    Object o -> Object $ o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

selectConjunction :: Text -> String -> Text -> Text -> Text -> ConjunctionValue
selectConjunction g k c t a = ExportConjunctionValue $ getConjunction g k c t a

-- -----------------------------------------------------------------
-- Value build

getConjunction :: Text -> String -> Text -> Text -> Text -> ConjunctionStructure
getConjunction g k c t a = ConjunctionStructure { vGroup = g
                      , vKeyword = k
                      , vConjunction = c
                      , vMType = t
                      , vAdjective = a}
