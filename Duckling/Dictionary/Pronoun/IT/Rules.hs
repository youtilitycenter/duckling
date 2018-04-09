-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.Pronoun.IT.Rules
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
import Duckling.Dictionary.Pronoun.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Dictionary.Pronoun.Types (PronounData(..))
import qualified Duckling.Numeral.Types as TNumeral

pronouns :: [(Text, String)]
pronouns =
  [ ("alcune", "(alcune)")
  , ("alcuni", "(alcuni)")
  , ("altra", "(altra)")
  , ("altre", "(altre)")
  , ("altri", "(altri)")
  , ("altro", "(altro)")
  , ("ciascuna", "(ciascuna)")
  , ("ciascuno", "(ciascuno)")
  , ("cio'", "(cio')")
  , ("ciò", "(ciò)")
  , ("colei", "(colei)")
  , ("coloro", "(coloro)")
  , ("colui", "(colui)")
  , ("costei", "(costei)")
  , ("costoro", "(costoro)")
  , ("costui", "(costui)")
  , ("egli", "(egli)")
  , ("entrambi", "(entrambi)")
  , ("essa", "(essa)")
  , ("esse", "(esse)")
  , ("essi", "(essi)")
  , ("esso", "(esso)")
  , ("io", "(io)")
  , ("lei", "(lei)")
  , ("loro", "(loro)")
  , ("lui", "(lui)")
  , ("me", "(me)")
  , ("mia", "(mia)")
  , ("mie", "(mie)")
  , ("miei", "(miei)")
  , ("mio", "(mio)")
  , ("molti", "(molti)")
  , ("nessuna", "(nessuna)")
  , ("nessuno", "(nessuno)")
  , ("niente", "(niente)")
  , ("noi", "(noi)")
  , ("nostra", "(nostra)")
  , ("nostre", "(nostre)")
  , ("nostri", "(nostri)")
  , ("nostro", "(nostro)")
  , ("novant'", "(novant')")
  , ("novanta", "(novanta)")
  , ("ognuna", "(ognuna)")
  , ("ognuno", "(ognuno)")
  , ("parecchi", "(parecchi)")
  , ("po'", "(po')")
  , ("poche", "(poche)")
  , ("pochi", "(pochi)")
  , ("poco", "(poco)")
  , ("propri", "(propri)")
  , ("propria", "(propria)")
  , ("proprie", "(proprie)")
  , ("proprio", "(proprio)")
  , ("pò", "(pò)")
  , ("qualcosa", "(qualcosa)")
  , ("qualcuna", "(qualcuna)")
  , ("qualcuno", "(qualcuno)")
  , ("quella", "(quella)")
  , ("quelle", "(quelle)")
  , ("quelli", "(quelli)")
  , ("quello", "(quello)")
  , ("questa", "(questa)")
  , ("queste", "(queste)")
  , ("questi", "(questi)")
  , ("questo", "(questo)")
  , ("sua", "(sua)")
  , ("sue", "(sue)")
  , ("suo", "(suo)")
  , ("suoi", "(suoi)")
  , ("tante", "(tante)")
  , ("tanti", "(tanti)")
  , ("te", "(te)")
  , ("troppi", "(troppi)")
  , ("tu", "(tu)")
  , ("tua", "(tua)")
  , ("tue", "(tue)")
  , ("tuo", "(tuo)")
  , ("tuoi", "(tuoi)")
  , ("tutte", "(tutte)")
  , ("tutti", "(tutti)")
  , ("tutto", "(tutto)")
  , ("voi", "(voi)")
  , ("vostra", "(vostra)")
  , ("vostre", "(vostre)")
  , ("vostri", "(vostri)")
  , ("vostro", "(vostro)")
  ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go pronouns
  where
    go :: (Text, String) -> Rule
    go (name, regexPattern) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Pronoun $ pronounHelper name
        _ -> Nothing
      }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleNumeralQuantities
