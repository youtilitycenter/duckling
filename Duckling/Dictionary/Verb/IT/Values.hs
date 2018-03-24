module Duckling.Dictionary.Verb.IT.Values
  ( verbsValue
  ) where

import Data.String
import Data.Text (Text)

verbsValue :: [(Text, String)]
verbsValue =
  [ ("volare", "(volare|vol(e|a|ò|o|i)(rò$|rai$|rà$|remo$|rete$|ranno$|i$|sti$|mmo$|ste$|rono$|to$|ta$|ti$|te$|amo$|te$|no$|vo$|vi$|va$|vamo$|vate$|vano$|rei$|resti$|rebbe$|remmo$|reste$|rebbero$)?)")
  , ("trovare", "(trovare|trov(e|a|ò|o|i)(rò$|rai$|rà$|remo$|rete$|ranno$|i$|sti$|mmo$|ste$|rono$|to$|ta$|ti$|te$|amo$|te$|no$|vo$|vi$|va$|vamo$|vate$|vano$|rei$|resti$|rebbe$|remmo$|reste$|rebbero$)?)")
  ]
