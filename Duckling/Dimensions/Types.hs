-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}


module Duckling.Dimensions.Types
  ( Some(..)
  , Dimension(..)

  , fromName
  , toName
  ) where

import Data.Maybe
import Data.Some
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.AmountOfMoney.Types (AmountOfMoneyData)
import Duckling.Distance.Types (DistanceData)
import Duckling.Duration.Types (DurationData)
import Duckling.Email.Types (EmailData)
import Duckling.Numeral.Types (NumeralData)
import Duckling.Ordinal.Types (OrdinalData)
import Duckling.PhoneNumber.Types (PhoneNumberData)
import Duckling.Quantity.Types (QuantityData)
import Duckling.Regex.Types (GroupMatch)
import Duckling.Temperature.Types (TemperatureData)
import Duckling.Time.Types (TimeData)
import Duckling.TimeGrain.Types (Grain)
import Duckling.Url.Types (UrlData)
import Duckling.Volume.Types (VolumeData)
import Duckling.Dictionary.Verb.Types (VerbData)
import Duckling.Dictionary.Semantic.Types (SemanticData)
import Duckling.Dictionary.SemanticNP.Types (SemanticDataNP)
import Duckling.Dictionary.SemanticVP.Types (SemanticDataVP)
import Duckling.Dictionary.SemanticPP.Types (SemanticDataPP)
import Duckling.Dictionary.Article.Types (ArticleData)
import Duckling.Dictionary.Adverb.Types (AdverbData)
import Duckling.Dictionary.Conjunction.Types (ConjunctionData)
import Duckling.Dictionary.Pronoun.Types (PronounData)
import Duckling.Dictionary.Preposition.Types (PrepositionData)
import Duckling.Dictionary.Adjective.Types (AdjectiveData)
import Duckling.Types

-- -----------------------------------------------------------------
-- Dimension

-- | GADT for differentiating between dimensions
-- Each dimension should have its own constructor and provide the data structure
-- for its parsed data
data Dimension a where
  RegexMatch :: Dimension GroupMatch
  AmountOfMoney :: Dimension AmountOfMoneyData
  Distance :: Dimension DistanceData
  Duration :: Dimension DurationData
  Email :: Dimension EmailData
  Numeral :: Dimension NumeralData
  Ordinal :: Dimension OrdinalData
  PhoneNumber :: Dimension PhoneNumberData
  Quantity :: Dimension QuantityData
  Temperature :: Dimension TemperatureData
  Time :: Dimension TimeData
  TimeGrain :: Dimension Grain
  Url :: Dimension UrlData
  Volume :: Dimension VolumeData
  Verb :: Dimension VerbData
  Semantic :: Dimension SemanticData
  SemanticNP :: Dimension SemanticDataNP
  SemanticVP :: Dimension SemanticDataVP
  SemanticPP :: Dimension SemanticDataPP
  Article :: Dimension ArticleData
  Adverb :: Dimension AdverbData
  Conjunction :: Dimension ConjunctionData
  Pronoun :: Dimension PronounData
  Preposition :: Dimension PrepositionData
  Adjective :: Dimension AdjectiveData

-- Show
instance Show (Dimension a) where
  show RegexMatch = "RegexMatch"
  show Distance = "Distance"
  show Duration = "Duration"
  show Email = "Email"
  show AmountOfMoney = "AmountOfMoney"
  show Numeral = "Numeral"
  show Ordinal = "Ordinal"
  show PhoneNumber = "PhoneNumber"
  show Quantity = "Quantity"
  show Temperature = "Temperature"
  show Time = "Time"
  show TimeGrain = "TimeGrain"
  show Url = "Url"
  show Volume = "Volume"
  show Verb = "Verb"
  show Semantic = "Semantic"
  show SemanticNP = "SemanticNP"
  show SemanticVP = "SemanticVP"
  show SemanticPP = "SemanticPP"
  show Article = "Article"
  show Adverb = "Adverb"
  show Conjunction = "Conjunction"
  show Pronoun = "Pronoun"
  show Preposition = "Preposition"
  show Adjective = "Adjective"
instance GShow Dimension where gshowsPrec = showsPrec

-- TextShow
instance TextShow (Dimension a) where
  showb d = TS.fromString $ show d
instance TextShow (Some Dimension) where
  showb (This d) = showb d

-- Hashable
instance Hashable (Some Dimension) where
  hashWithSalt s (This a) = hashWithSalt s a
instance Hashable (Dimension a) where
  hashWithSalt s RegexMatch  = hashWithSalt s (0::Int)
  hashWithSalt s Distance    = hashWithSalt s (1::Int)
  hashWithSalt s Duration    = hashWithSalt s (2::Int)
  hashWithSalt s Email       = hashWithSalt s (3::Int)
  hashWithSalt s AmountOfMoney     = hashWithSalt s (4::Int)
  hashWithSalt s Numeral     = hashWithSalt s (5::Int)
  hashWithSalt s Ordinal     = hashWithSalt s (6::Int)
  hashWithSalt s PhoneNumber = hashWithSalt s (7::Int)
  hashWithSalt s Quantity    = hashWithSalt s (8::Int)
  hashWithSalt s Temperature = hashWithSalt s (9::Int)
  hashWithSalt s Time        = hashWithSalt s (10::Int)
  hashWithSalt s TimeGrain   = hashWithSalt s (11::Int)
  hashWithSalt s Url         = hashWithSalt s (12::Int)
  hashWithSalt s Volume      = hashWithSalt s (13::Int)
  hashWithSalt s Verb        = hashWithSalt s (14::Int)
  hashWithSalt s Semantic    = hashWithSalt s (15::Int)
  hashWithSalt s SemanticNP  = hashWithSalt s (16::Int)
  hashWithSalt s SemanticVP  = hashWithSalt s (17::Int)
  hashWithSalt s SemanticPP  = hashWithSalt s (18::Int)
  hashWithSalt s Article     = hashWithSalt s (19::Int)
  hashWithSalt s Adverb      = hashWithSalt s (20::Int)
  hashWithSalt s Conjunction = hashWithSalt s (21::Int)
  hashWithSalt s Pronoun     = hashWithSalt s (22::Int)
  hashWithSalt s Preposition = hashWithSalt s (23::Int)
  hashWithSalt s Adjective   = hashWithSalt s (24::Int)


toName :: Dimension a -> Text
toName RegexMatch = "regex"
toName Distance = "distance"
toName Duration = "duration"
toName Email = "email"
toName AmountOfMoney = "amount-of-money"
toName Numeral = "number"
toName Ordinal = "ordinal"
toName PhoneNumber = "phone-number"
toName Quantity = "quantity"
toName Temperature = "temperature"
toName Time = "time"
toName TimeGrain = "time-grain"
toName Url = "url"
toName Volume = "volume"
toName Verb = "verb"
toName Semantic = "semantic"
toName SemanticNP = "semantic_np"
toName SemanticVP = "semantic_vp"
toName SemanticPP = "semantic_pp"
toName Article = "article"
toName Adverb = "adverb"
toName Conjunction = "conjunction"
toName Pronoun = "pronoun"
toName Preposition = "preposition"
toName Adjective = "adjective"
toName (CustomDimension dim) = Text.pack (show dim)

fromName :: Text -> Maybe (Some Dimension)
fromName name = HashMap.lookup name m
  where
    m = HashMap.fromList
      [ ("amount-of-money", This AmountOfMoney)
      , ("distance", This Distance)
      , ("duration", This Duration)
      , ("email", This Email)
      , ("number", This Numeral)
      , ("ordinal", This Ordinal)
      , ("phone-number", This PhoneNumber)
      , ("quantity", This Quantity)
      , ("temperature", This Temperature)
      , ("time", This Time)
      , ("url", This Url)
      , ("volume", This Volume)
      , ("verb", This Verb)
      , ("semantic", This Semantic)
      , ("semantic_np", This SemanticNP)
      , ("semantic_vp", This SemanticVP)
      , ("semantic_pp", This SemanticPP)
      , ("article", This Article)
      , ("adverb", This Adverb)
      , ("conjunction", This Conjunction)
      , ("pronoun", This Pronoun)
      , ("preposition", This Preposition)
      , ("adjective", This Adjective)
      ]
