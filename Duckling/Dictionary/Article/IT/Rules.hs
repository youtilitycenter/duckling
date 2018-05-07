-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.Article.IT.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Dictionary.Article.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Dictionary.Article.Types (ArticleData(..))
import qualified Duckling.Numeral.Types as TNumeral

articles :: [(Text, String)]
articles =
  [ ("il", "(il)")
  , ("lo", "(lo)")
  , ("l'", "(l'|l)")
  , ("i", "(i)")
  , ("gli", "(gli)")
  , ("la", "(la)")
  , ("le", "(le)")
  , ("un", "(un)")
  , ("uno", "(uno)")
  , ("una", "(una|un')")
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go articles
  where
    go :: (Text, String) -> Rule
    go (article, regexPattern) = Rule
      { name = article
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Article $ articleHelper article
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleNumeralQuantities
