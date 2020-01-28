module Network.HTTP.Media.Utils where

import Data.String.CaseInsensitive (CaseInsensitiveString(..))

mkCaseI :: String -> CaseInsensitiveString
mkCaseI = CaseInsensitiveString