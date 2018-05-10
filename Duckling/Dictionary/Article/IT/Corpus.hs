-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Dictionary.Article.IT.Corpus
  ( corpus ) where

import Duckling.Locale
import Duckling.Resolve
import Prelude
import Data.String

import Duckling.Dictionary.Article.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale IT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple "Il")
             [ "il cane"
             , "il gatto"
             ]
  ]
