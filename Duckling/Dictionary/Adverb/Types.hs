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

module Duckling.Dictionary.Adverb.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

data AdverbData = AdverbData
  { adverb :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON AdverbData where
    toJSON (AdverbData adverb) = object $
      ["value" .= adverb
      ]

instance Resolve AdverbData where
  type ResolvedValue AdverbData = AdverbValue

  resolve _ _ AdverbData {adverb = Just adverb}
   = Just (simple adverb, False)

data SingleValue = SingleValue
    { vAdverb :: Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SingleValue where
    toJSON (SingleValue adverb) = object $
      ["adverb" .= adverb
      ]

data AdverbValue
  = SimpleValue SingleValue
  deriving (Eq, Ord, Show)

instance ToJSON AdverbValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

simple :: Text -> AdverbValue
simple a = SimpleValue $ single a

single :: Text -> SingleValue
single a = SingleValue {vAdverb = a}
