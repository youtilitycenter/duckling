-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}

module Duckling.Dictionary.Helpers
  ( verbHelper
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Types (DictionaryData(..))
import Duckling.Types
import qualified Duckling.Dictionary.Types as TDictionary

-- -----------------------------------------------------------------
-- Production

verbHelper :: Text -> Text -> Text -> Text -> DictionaryData
verbHelper v f m p = DictionaryData {TDictionary.verb = Just v
                                  , TDictionary.form = Just f
                                  , TDictionary.mode = Just m
                                  , TDictionary.person = Just p}
