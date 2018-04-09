-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.Preposition.IT.Rules
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
import Duckling.Dictionary.Preposition.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Dictionary.Preposition.Types (PrepositionData(..))
import qualified Duckling.Numeral.Types as TNumeral

prepositions :: [(Text, String)]
prepositions =
  [ ("a", "(a)")
  , ("accanto", "(accanto)")
  , ("ad", "(ad)")
  , ("agli", "(agli)")
  , ("ai", "(ai)")
  , ("al", "(al)")
  , ("all'", "(all')")
  , ("alla", "(alla)")
  , ("alle", "(alle)")
  , ("allo", "(allo)")
  , ("assieme", "(assieme)")
  , ("attorno", "(attorno)")
  , ("attraverso", "(attraverso)")
  , ("a causa del", "(a causa del)")
  , ("a causa della", "(a causa della)")
  , ("a causa di", "(a causa di)")
  , ("a favore del", "(a favore del)")
  , ("a favore di", "(a favore di)")
  , ("a meno che", "(a meno che)")
  , ("a norma degli", "(a norma degli)")
  , ("a norma del", "(a norma del)")
  , ("a norma dell'", "(a norma dell')")
  , ("a norma della", "(a norma della)")
  , ("a norma delle", "(a norma delle)")
  , ("al di lá", "(al di lá)")
  , ("al di sopra di", "(al di sopra di)")
  , ("causa", "(causa)")
  , ("circa", "(circa)")
  , ("cogli", "(cogli)")
  , ("coi", "(coi)")
  , ("col", "(col)")
  , ("con", "(con)")
  , ("contro", "(contro)")
  , ("d'", "(d')")
  , ("da", "(da)")
  , ("dagli", "(dagli)")
  , ("dai", "(dai)")
  , ("dal", "(dal)")
  , ("dall'", "(dall')")
  , ("dalla", "(dalla)")
  , ("dalle", "(dalle)")
  , ("dallo", "(dallo)")
  , ("davanti", "(davanti)")
  , ("de", "(de)")
  , ("degli", "(degli)")
  , ("dei", "(dei)")
  , ("del", "(del)")
  , ("dell'", "(dell')")
  , ("della", "(della)")
  , ("delle", "(delle)")
  , ("dello", "(dello)")
  , ("dentro", "(dentro)")
  , ("di", "(di)")
  , ("dietro", "(dietro)")
  , ("dinanzi", "(dinanzi)")
  , ("dopo", "(dopo)")
  , ("durante", "(durante)")
  , ("entro", "(entro)")
  , ("fin", "(fin)")
  , ("fino", "(fino)")
  , ("fra", "(fra)")
  , ("fuori", "(fuori)")
  , ("in", "(in)")
  , ("innanzi", "(innanzi)")
  , ("insieme", "(insieme)")
  , ("intorno", "(intorno)")
  , ("in base all'", "(in base all')")
  , ("in mano ai", "(in mano ai)")
  , ("in mano alla", "(in mano alla)")
  , ("in vista del", "(in vista del)")
  , ("in vista dell'", "(in vista dell')")
  , ("in vista della", "(in vista della)")
  , ("lungo", "(lungo)")
  , ("mediante", "(mediante)")
  , ("negli", "(negli)")
  , ("nei", "(nei)")
  , ("nel", "(nel)")
  , ("nell'", "(nell')")
  , ("nella", "(nella)")
  , ("nelle", "(nelle)")
  , ("nello", "(nello)")
  , ("nonostante", "(nonostante)")
  , ("nei confronti dei", "(nei confronti dei)")
  , ("nei confronti del", "(nei confronti del)")
  , ("nei confronti dell'", "(nei confronti dell')")
  , ("nei confronti di", "(nei confronti di)")
  , ("oltre", "(oltre)")
  , ("per", "(per)")
  , ("presso", "(presso)")
  , ("pro", "(pro)")
  , ("rispetto", "(rispetto)")
  , ("secondo", "(secondo)")
  , ("senz'", "(senz')")
  , ("senza", "(senza)")
  , ("sino", "(sino)")
  , ("sopra", "(sopra)")
  , ("sott'", "(sott')")
  , ("sotto", "(sotto)")
  , ("su", "(su)")
  , ("sugli", "(sugli)")
  , ("sui", "(sui)")
  , ("sul", "(sul)")
  , ("sull'", "(sull')")
  , ("sulla", "(sulla)")
  , ("sulle", "(sulle)")
  , ("sullo", "(sullo)")
  , ("tra", "(tra)")
  , ("tramite", "(tramite)")
  , ("tranne", "(tranne)")
  , ("verso", "(verso)")
  , ("via", "(via)")
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go prepositions
  where
    go :: (Text, String) -> Rule
    go (name, regexPattern) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Preposition $ prepositionHelper name
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleNumeralQuantities
