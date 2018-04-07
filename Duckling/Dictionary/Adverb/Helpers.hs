-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}


module Duckling.Dictionary.Adverb.Helpers
  ( adverbHelper
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Adverb.Types (AdverbData(..))
import Duckling.Types
import qualified Duckling.Dictionary.Adverb.Types as TAdverb

adverbHelper :: Text -> AdverbData
adverbHelper a = AdverbData {TAdverb.adverb = Just a}
