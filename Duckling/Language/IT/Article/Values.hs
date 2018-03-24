module Duckling.Article.IT.Values
  ( article
  ) where

import Data.String
import Data.Text (Text)

article :: [(Text, String)]
article =
  [ ("il", "(il)$")
  , ("lo", "(lo)$")
  , ("il", "(l'?)$")
  ]
