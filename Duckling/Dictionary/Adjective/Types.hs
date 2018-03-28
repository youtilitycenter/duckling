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

module Duckling.Dictionary.Adjective.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

data Adjective
  = Il
  | Lo
  | L
  | I
  | Gli
  | La
  | Le
  | Un
  | Uno
  | Una
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON Adjective where
  toJSON x = String . Text.toLower . Text.pack $ show x

data AdjectiveData = AdjectiveData
  { adjective :: Maybe Adjective
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON AdjectiveData where
    toJSON (AdjectiveData adjective) = object $
      ["value" .= adjective
      ]

instance Resolve AdjectiveData where
  type ResolvedValue AdjectiveData = AdjectiveValue

  resolve _ AdjectiveData {adjective = Just adjective}
   = Just $ simple adjective

data SingleValue = SingleValue
    { vAdjective :: Adjective
    }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON SingleValue where
    toJSON (SingleValue adjective) = object $
      ["adjective" .= adjective
      ]

data AdjectiveValue
  = SimpleValue SingleValue
  deriving (Eq, Ord, Show)

instance ToJSON AdjectiveValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _ -> Object H.empty

-- -----------------------------------------------------------------
-- Value helpers

simple :: Adjective -> AdjectiveValue
simple a = SimpleValue $ single a

single :: Adjective -> SingleValue
single a = SingleValue {vAdjective = a}
