-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}

module Duckling.Dictionary.Verb.Helpers
  ( verbHelper
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Verb.Types (VerbData(..))
import Duckling.Types
import qualified Duckling.Dictionary.Verb.Types as TDictionary

-- -----------------------------------------------------------------
-- Production

verbHelper :: Text -> Text -> Text -> VerbData
verbHelper v f p = VerbData {TDictionary.verb = Just v
                                  , TDictionary.form = Just f
                                  , TDictionary.person = Just p}
