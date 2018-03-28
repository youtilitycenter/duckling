-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}


module Duckling.Dictionary.Adjective.Helpers
  ( adjectiveHelper
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Dictionary.Adjective.Types (AdjectiveData(..))
import Duckling.Types
import qualified Duckling.Dictionary.Adjective.Types as TAdjective

adjectiveHelper :: TAdjective.Adjective -> AdjectiveData
adjectiveHelper a = AdjectiveData {TAdjective.adjective = Just a}
