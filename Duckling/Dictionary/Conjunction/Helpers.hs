-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}

module Duckling.Dictionary.Conjunction.Helpers
  ( conjunctionHelper
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Conjunction.Types (ConjunctionData(..))
import Duckling.Types
import qualified Duckling.Dictionary.Conjunction.Types as TDictionary

-- -----------------------------------------------------------------
-- Production

conjunctionHelper :: Text -> String -> Text -> Text -> Text -> ConjunctionData
conjunctionHelper g k c t a = ConjunctionData { TDictionary.group = g
                                  , TDictionary.keyword = k
                                  , TDictionary.conjunction = c
                                  , TDictionary.mtype = t
                                  , TDictionary.adjective = Just a}
