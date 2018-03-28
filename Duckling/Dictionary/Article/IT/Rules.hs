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
import qualified Duckling.Dictionary.Article.Types as TArticle

articles :: [(Text, String, TArticle.Article)]
articles =
  [ ("il", "(il)", TArticle.Il)
  , ("lo", "(lo)", TArticle.Lo)
  , ("l'", "(l'|l)", TArticle.L)
  , ("i", "(i)", TArticle.I)
  , ("gli", "(gli)", TArticle.Gli)
  , ("la", "(la)", TArticle.La)
  , ("le", "(le)", TArticle.Le)
  , ("un", "(un)", TArticle.Un)
  , ("uno", "(uno)", TArticle.Uno)
  , ("una", "(una|un')", TArticle.Una)
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go articles
  where
    go :: (Text, String, TArticle.Article) -> Rule
    go (name, regexPattern, a) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Article $ articleHelper a
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleNumeralQuantities
