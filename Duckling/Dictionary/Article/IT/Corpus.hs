-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Dictionary.Article.IT.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Dictionary.Article.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Il)
             [ "il cane"
             , "il gatto"
             ]
    , examples (simple Lo)
              [ "lo struzzo"
              , "lo striscione"
              ]
    , examples (simple L)
              [ "l'anatroccolo"
              , "l'altare"
              ]
    , examples (simple I)
               [ "i cani"
               , "i gatti"
               ]
    , examples (simple Gli)
              [ "gli animali"
              , "gli struzzi"
              ]
    , examples (simple La)
             [ "la casa"
             , "la statua"
             ]
    , examples (simple Le)
            [ "le donne"
            , "le case"
            ]
    , examples (simple Un)
            [ "un castello"
            , "un giardino"
            ]
    , examples (simple Uno)
            [ "uno struzzo"
            , "uno striscione"
            ]
    , examples (simple Una)
            [ "una donna"
            , "una casa"
            ]
  ]
