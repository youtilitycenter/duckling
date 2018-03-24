module Duckling.Language.IT.H
  ( h
  ) where

import Data.String
import Data.Text (Text)

h :: [(Text, String)]
h =
  [ ("handicappare", "(handicappare|handicapp(e|a|ò|o|i)(rò$|rai$|rà$|remo$|rete$|ranno$|i$|sti$|mmo$|ste$|rono$|to$|ta$|ti$|te$|amo$|te$|no$|vo$|vi$|va$|vamo$|vate$|vano$|rei$|resti$|rebbe$|remmo$|reste$|rebbero$)?)")
  , ("volare", "(volare|vol(e|a|ò|o|i)(rò$|rai$|rà$|remo$|rete$|ranno$|i$|sti$|mmo$|ste$|rono$|to$|ta$|ti$|te$|amo$|te$|no$|vo$|vi$|va$|vamo$|vate$|vano$|rei$|resti$|rebbe$|remmo$|reste$|rebbero$)?)")
  , ("trovare", "(trovare|trov(e|a|ò|o|i)(rò$|rai$|rà$|remo$|rete$|ranno$|i$|sti$|mmo$|ste$|rono$|to$|ta$|ti$|te$|amo$|te$|no$|vo$|vi$|va$|vamo$|vate$|vano$|rei$|resti$|rebbe$|remmo$|reste$|rebbero$)?)")
  ]
