module Network.HTTP.Media.RenderHeader where

import Prelude

import Data.Foldable (intercalate)

class RenderHeader h where
    -- | Render a header value to as a UTF-8 String 
    renderHeader :: h -> String

instance renderHeaderString :: RenderHeader String where
    renderHeader = identity

instance renderHeader' :: RenderHeader h => RenderHeader (Array h) where
    renderHeader = intercalate "," <<< map renderHeader