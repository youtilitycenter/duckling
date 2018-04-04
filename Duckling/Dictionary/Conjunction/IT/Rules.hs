-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Dictionary.Conjunction.IT.Rules
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
import Duckling.Dictionary.Conjunction.Helpers
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Dictionary.Conjunction.Types (ConjunctionData(..))
import qualified Duckling.Numeral.Types as TNumeral

conjunctions :: [(Text, Text, String, String)]
conjunctions =
  [ ("subordinante", "causale",       "null", "(siccome che|poich(?:é|è|e)|in quanto che|giacch(?:è|é|e)|dacch(?:è|é|e)|dal momento che|per via che|visto che|dato che)()")
  , ("subordinante", "finale",        "null", "(affinch(?:é|è|e)|acciocch(?:é|è|e))()")
  , ("subordinante", "consecutiva",   "null", "(cosicch(?:é|è|e)|tanto che|in modo che|cos(?:i|ì) che)()")
  , ("subordinante", "temporale",     "null", "(finch(?:é|è|e)|fin quando|fintantoch(?:é|è|e)|da che|da quando|dopo che|prima che|intanto che|non appena|appena|ogni qual volta|ogni volta che|ora che|mentre)()")
  , ("subordinante", "concessiva",    "null", "(anche se|anche quando|qualora|bench(?:é|è|e)|sebbene|quantunque)()")
  , ("subordinante", "dichiarativa",  "null", "(?(?!siccome|in quanto|dal momento|per via|visto|dato|modo|da|dopo|prima|intanto|volta|condizione|tranne|eccetto|salvo|meno|senza|quello)(che))()")
  , ("subordinante", "condizionale",  "null", "((?(?!come|anche)se)|qualora|purch(?:é|è|e)|a condizione che|a patto che|laddove)()")
  , ("subordinante", "modale",        "null", "(come se|nel modo che)()")
  , ("subordinante", "limitativa",    "null", "(ma non|tranne che|fuorch(?:é|è|e)|eccetto che|salvo che|a meno che|senza che|per quello che)()")
  , ("subordinante", "interrogativa", "null", "((?(?!fin|anche|da)quando)|(?(?!in|di)quanto)|perch(?:é|è|e))()")
  , ("subordinante", "comparativa",   "null", "((?:più|piu|meno)(\\s\\w+)? di (?:come|quanto))()")
  , ("subordinante", "comparativa",   "null", "((?:più|piu|meno|meglio|peggio)(\\s\\w+)? di quello(?:(?:\\s)che)?|piuttosto che)()")

  , ("subordinante_coordinante", "concessiva_avversativa", "null", "(nonostante)()")

  , ("coordinante", "copulativa_positiva",  "and",          "(e|(?(?!quando|se)anche)|pure|inoltre|ancora|perfino|altres(?:ì|i))()")
  , ("coordinante", "copulativa_negativa",  "nor",          "(n(?:é|è|e)|neanche|neppure|nemmeno)()")
  , ("coordinante", "disgiuntiva",          "xor",          "(o|oppure|altrimenti)()")
  , ("coordinante", "avversativa",          "opposition",   "((?(?!ma non)ma)|tuttavia|per(?:ò|o)|eppure|anzi|nondimeno|bens(?:ì|i)|piuttosto|invece|se non che|al contrario|per altro|ci(?:ò|o) nonostante)()")
  , ("coordinante", "conclusiva",           "end",          "(dunque|perci(?:o|ò)|quindi|pertanto|allora|per cui|insomma)()")
  , ("coordinante", "dichiarativa",         "confirmation", "(infatti|difatti|invero|cio(?:e|è|é)|ossia|ovvero|vale a dire|in effetti|effettivamente|in realt(?:a|à))()")
  , ("coordinante", "correlativa",          "join",         "(sia|non solo|ma anche|tanto quanto|tale|quale|cos(?:ì|i) come|(?(?!se)come)|sia che)()")
  ]

ruleConjunctions :: [Rule]
ruleConjunctions = map go conjunctions
  where
    go :: (Text, Text, String, String) -> Rule
    go (group, mtype, keyword, regexPattern) = Rule
      { name = mtype
      , pattern = [regex regexPattern]
      , prod = \case
        (Token RegexMatch (GroupMatch (conjunction:adjective:_ )):
         _) -> Just . Token Conjunction $ conjunctionHelper group keyword conjunction mtype adjective
        _ -> Nothing
      }



rules :: [Rule]
rules =
  [
  ]
  ++ ruleConjunctions
