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

module Duckling.Dictionary.Verb.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

data VerbData = VerbData
  { verb :: Maybe Text
  , form :: Maybe Text
  , person :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve VerbData where
  type ResolvedValue VerbData = VerbValue

  resolve _ VerbData {verb = Just verb
                            , form = Just form
                            , person = Just person}
   = Just $ selectVerb verb form person

  resolve _ _ = Nothing

instance ToJSON VerbData where
    toJSON (VerbData verb form person) = object $
      [ "lemma" .= verb
      , "form" .= form
      , "person" .= person
      ]

data VerbStructure = VerbStructure
    { vVerb :: Text
    , vForm :: Text
    , vPerson :: Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON VerbStructure where
    toJSON (VerbStructure verb form person) = object $
      [ "lemma" .= verb
      , "form" .= form
      , "person" .= person
      ]

data VerbValue
  = ExportVerbValue VerbStructure
  deriving (Eq, Ord, Show)

instance ToJSON VerbValue where
  toJSON (ExportVerbValue value) = case toJSON value of
    Object o -> Object $ o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

selectVerb :: Text -> Text -> Text -> VerbValue
selectVerb u f p = ExportVerbValue $ getVerb u f p

-- -----------------------------------------------------------------
-- Value build

getVerb :: Text -> Text -> Text -> VerbStructure
getVerb u f p = VerbStructure {vVerb = u
                      , vForm = f
                      , vPerson = p}
