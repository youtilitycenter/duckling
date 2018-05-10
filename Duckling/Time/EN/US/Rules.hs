-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.US.Rules
  ( rules
  , rulesBackwardCompatible
  ) where

import Data.Maybe
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Time.Types as TTime

ruleMMDD :: Rule
ruleMMDD = Rule
  { name = "mm/dd"
  , pattern =
    [ regex "(1[0-2]|0?[1-9])\\s?[/-]\\s?(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleMMDDYYYY :: Rule
ruleMMDDYYYY = Rule
  { name = "mm/dd/yyyy"
  , pattern =
    [ regex "(1[0-2]|0?[1-9])[/-](3[01]|[12]\\d|0?[1-9])[/-](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-- Clashes with HHMMSS, hence only 4-digit years
ruleMMDDYYYYDot :: Rule
ruleMMDDYYYYDot = Rule
  { name = "mm.dd.yyyy"
  , pattern =
    [ regex "(1[0-2]|0?[1-9])\\.(3[01]|[12]\\d|0?[1-9])\\.(\\d{4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mm:dd:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-- Fourth Thursday of November
ruleThanksgiving :: Rule
ruleThanksgiving = Rule
  { name = "Thanksgiving Day"
  , pattern =
    [ regex "thanks?giving( day)?"
    ]
  , prod = const . tt . mkOkForThisNext $ nthDOWOfMonth 4 4 11
  }

ruleKwanzaa :: Rule
ruleKwanzaa  = Rule
  { name = "Kwanzaa"
  , pattern =
    [ regex "kwanzaa"
    ]
  , prod = const $ Token Time <$> mkOkForThisNext
      <$> interval TTime.Open (monthDay 12 26) (monthDay 1 1)
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  -- Fixed dates, year over year
  [ ( "African Liberation Day", "african liberation day", monthDay 5 25 )
  , ( "Air Force Birthday", "air force birthday", monthDay 9 18 )
  , ( "Alaska Day", "alaska day", monthDay 10 18 )
  , ( "American Eagle Day", "american eagle day", monthDay 6 20 )
  , ( "Army Birthday", "army birthday", monthDay 6 14 )
  , ( "Bennington Battle Day", "bennington battle day", monthDay 8 16 )
  , ( "Bunker Hill Day", "bunker hill day", monthDay 6 17 )
  , ( "California Admission Day", "california admission day", monthDay 9 9 )
  , ( "Cinco de Mayo", "cinco de mayo", monthDay 5 5 )
  , ( "Citizenship Day", "citizenship day", monthDay 9 17 )
  , ( "Coast Guard Birthday", "coast guard birthday", monthDay 8 4 )
  , ( "Colorado Day", "colorado day", monthDay 8 1 )
  , ( "Constitution Day and Citizenship Day", "constitution day and citizenship day", monthDay 9 17 )
  , ( "César Chávez Day", "c[ée]sar ch[áa]vez day", monthDay 3 31 )
  , ( "D-Day", "d\\-day", monthDay 6 6 )
  , ( "Day After Christmas Day", "day after christmas day", monthDay 12 26 )
  , ( "Elizabeth Peratrovich Day", "elizabeth peratrovich day", monthDay 2 16 )
  , ( "Emancipation Day", "emancipation day", monthDay 4 16 )
  , ( "Evacuation Day", "evacuation day", monthDay 3 17 )
  , ( "Father Damien Day", "father damien day", monthDay 4 15 )
  , ( "Feast of Our Lady of Guadalupe", "feast of our lady of guadalupe", monthDay 12 12 )
  , ( "Flag Day", "flag day", monthDay 6 14 )
  , ( "Groundhog Day", "groundhog day", monthDay 2 2 )
  , ( "Harvey Milk Day", "harvey milk day", monthDay 5 22 )
  , ( "Inauguration Day", "inauguration day", monthDay 1 20 )
  , ( "Independence Day", "independence day", monthDay 7 4 )
  , ( "Juneteenth", "juneteenth", monthDay 6 19 )
  , ( "Kamehameha Day", "kamehameha day", monthDay 6 11 )
  , ( "Kansas Day", "kansas day", monthDay 1 29 )
  , ( "Kent State Shootings Remembrance", "kent state shootings remembrance", monthDay 5 4 )
  , ( "Law/Lei/Loyalty Day", "l(aw|ei|oyalty) day", monthDay 5 1 )
  , ( "Leif Erikson Day", "leif erikson day", monthDay 10 9 )
  , ( "Linus Pauling Day", "linus pauling day", monthDay 2 28 )
  , ( "Lyndon Baines Johnson Day", "lyndon baines johnson day", monthDay 8 27 )
  , ( "Marine Corps Birthday", "marine corps birthday", monthDay 11 10 )
  , ( "Maryland Day", "maryland day", monthDay 3 25 )
  , ( "National Aviation Day", "national aviation day", monthDay 8 19 )
  , ( "National Freedom Day", "national freedom day", monthDay 2 1 )
  , ( "National Guard Birthday", "national guard birthday", monthDay 12 13 )
  , ( "National Korean War Veterans Armistice Day", "national korean war veterans armistice day", monthDay 7 27 )
  , ( "National Maritime Day", "national maritime day", monthDay 5 22 )
  , ( "National Missing Children's Day", "national missing children'?s day", monthDay 5 25 )
  , ( "National Nurses Day", "national nurses day", monthDay 5 6 )
  , ( "National Tartan Day", "national tartan day", monthDay 4 6 )
  , ( "Navy Birthday", "navy birthday", monthDay 10 13 )
  , ( "Oklahoma Day", "oklahoma day", monthDay 4 22 )
  , ( "Pan American Aviation Day", "pan american aviation day", monthDay 12 17 )
  , ( "Pascua Florida Day", "pascua florida day", monthDay 4 2 )
  , ( "Patriot Day", "patriot day", monthDay 9 11 )
  , ( "Peace Officers Memorial Day", "peace officers memorial day", monthDay 5 15 )
  , ( "Pearl Harbor Remembrance Day", "pearl harbor remembrance day", monthDay 12 7 )
  , ( "Pioneer Day", "pioneer day", monthDay 7 24 )
  , ( "Prince Jonah Kuhio Kalanianaole Day", "prince jonah kuhio kalanianaole day", monthDay 3 26 )
  , ( "Purple Heart Day", "purple heart day", monthDay 8 7 )
  , ( "Rhode Island Independence Day", "rhode island independence day", monthDay 5 4 )
  , ( "Rosa Parks Day", "rosa parks day", monthDay 2 4 )
  , ( "San Jacinto Day", "san jacinto day", monthDay 4 21 )
  , ( "Self-Injury Awareness Day", "self\\-injury awareness day", monthDay 3 1 )
  , ( "Senior Citizens Day", "senior citizens day", monthDay 8 21 )
  , ( "St Nicholas' Day", "st\\.? nicholas'? day", monthDay 12 6 )
  , ( "St. David's Day", "st\\.? david'?s day", monthDay 3 1 )
  , ( "Statehood Day", "statehood day", monthDay 6 1 )
  , ( "Statehood Day in Arizona", "statehood day in arizona", monthDay 2 14 )
  , ( "Stephen Foster Memorial Day", "stephen foster memorial day", monthDay 1 13 )
  , ( "Susan B Anthony's Birthday", "susan b\\.? anthony'?s birthday", monthDay 2 15 )
  , ( "Texas Independence Day", "texas independence day", monthDay 3 2 )
  , ( "Thomas Jefferson's Birthday", "thomas jefferson'?s birthday", monthDay 4 13 )
  , ( "Truman Day", "truman day", monthDay 5 8 )
  , ( "Veterans Day", "veterans day", monthDay 11 11 )
  , ( "West Virginia Day", "west virginia day", monthDay 6 20 )
  , ( "White Cane Safety Day", "white cane safety day", monthDay 10 15 )
  , ( "Women's Equality Day", "women'?s equality day", monthDay 8 26 )
  , ( "Wright Brothers Day", "wright brothers day", monthDay 12 17 )

  -- Last Monday of May
  , ( "Memorial Day", "(decoration|memorial) day", predLastOf (dayOfWeek 1) (month 5) )
  -- Long weekend before the last Monday of May
  , ( "Memorial Day weekend", "(decoration|memorial) day week(\\s|-)?ends?"
    , longWEBefore $ predLastOf (dayOfWeek 1) (month 5)
    )
  ]

rulesBackwardCompatible :: [Rule]
rulesBackwardCompatible =
  [ ruleMMDD
  , ruleMMDDYYYY
  , ruleMMDDYYYYDot
  , ruleThanksgiving
  ]

rules :: [Rule]
rules =
 [ ruleKwanzaa
 ]
 ++ rulesBackwardCompatible
 ++ rulePeriodicHolidays
