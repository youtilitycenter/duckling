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

module Duckling.Dictionary.Preposition.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

data PrepositionData = PrepositionData
  { preposition :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON PrepositionData where
    toJSON (PrepositionData preposition) = object $
      ["value" .= preposition
      ]

instance Resolve PrepositionData where
  type ResolvedValue PrepositionData = PrepositionValue

  resolve _ PrepositionData {preposition = Just preposition}
   = Just $ simple preposition

data SingleValue = SingleValue
    { vPreposition :: Text
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SingleValue where
    toJSON (SingleValue preposition) = object $
      ["preposition" .= preposition
      ]

data PrepositionValue
  = SimpleValue SingleValue
  deriving (Eq, Ord, Show)

instance ToJSON PrepositionValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

simple :: Text -> PrepositionValue
simple a = SimpleValue $ single a

single :: Text -> SingleValue
single a = SingleValue {vPreposition = a}
