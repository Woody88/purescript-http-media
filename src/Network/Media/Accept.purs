module Network.Media.Accept where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CaseInsensitiveString(..))

class Show a <= Accept a where
    parseAccept :: String -> Maybe a 

    matches :: a -> a -> Boolean


instance acceptString :: Accept String where 
    parseAccept = Just 
    matches a b = CaseInsensitiveString a == CaseInsensitiveString b