module Network.HTTP.Media.Accept where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CaseInsensitiveString(..))

class Show a <= Accept a where
    parseAccept :: String -> Maybe a 
    matches :: a -> a -> Boolean
    moreSpecificThan :: a -> a -> Boolean
    hasExtensionParameters :: forall proxy. proxy a -> Boolean

instance acceptString :: Accept String where 
    parseAccept = Just 
    matches a b = CaseInsensitiveString a == CaseInsensitiveString b
    moreSpecificThan _ _ = false
    hasExtensionParameters _ = false