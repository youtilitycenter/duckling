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

module Duckling.Dictionary.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

data DictionaryData = DictionaryData
  { verb :: Maybe Text
  , form :: Maybe Text
  , mode :: Maybe Text
  , person :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve DictionaryData where
  type ResolvedValue DictionaryData = DictionaryValue

  resolve _ DictionaryData {verb = Just verb
                            , form = Just form
                            , mode = Just mode
                            , person = Just person}
   = Just $ selectVerb verb form mode person

  resolve _ _ = Nothing

data VerbValue = VerbValue
    { vVerb :: Text
    , vForm :: Text
    , vMode :: Text
    , vPerson :: Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON VerbValue where
    toJSON (VerbValue verb form mode person) = object $
      [ "lemma" .= verb
      , "form" .= form
      , "mode" .= mode
      , "person" .= person
      ]

data DictionaryValue
  = ExportVerbValue VerbValue
  deriving (Eq, Ord, Show)

instance ToJSON DictionaryValue where
  toJSON (ExportVerbValue value) = case toJSON value of
    Object o -> Object $ o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

selectVerb :: Text -> Text -> Text -> Text -> DictionaryValue
selectVerb u f m p = ExportVerbValue $ getVerb u f m p

-- -----------------------------------------------------------------
-- Value build

getVerb :: Text -> Text -> Text -> Text -> VerbValue
getVerb u f m p = VerbValue {vVerb = u
                      , vForm = f
                      , vMode = m
                      , vPerson = p}
