module Duckling.Dictionary.Verb.IT.Values.Irregolar
  ( verbs
  ) where

import Data.String
import Data.Text (Text)

verbs :: [(Text, String, String)]
verbs =
  [ ("handicappare", "(handicappare|handicapp(e|a|ò|o|i)(rò$|rai$|rà$|remo$|rete$|ranno$|i$|sti$|mmo$|ste$|rono$|to$|ta$|ti$|te$|amo$|te$|no$|vo$|vi$|va$|vamo$|vate$|vano$|rei$|resti$|rebbe$|remmo$|reste$|rebbero$)?)")
  , ("accendere", "accendo", "1s")
  , ("accendere", "accendo", "2s")
  , ("accendere", "accendo", "3s")
  , ("accendere", "accendo", "1p")
  , ("accendere", "accendo", "2p")
  , ("accendere", "accendo", "3p")
  ]
