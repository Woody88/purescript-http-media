module Network.Media.Quality where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Global as Global
import Network.HTTP.Media.RenderHeader (class RenderHeader, renderHeader)

-- | Attaches a quality value to data.
newtype Quality a = Quality { property :: a, value :: Number } 

instance renderHeaderQuality :: RenderHeader h => RenderHeader (Quality h) where
    renderHeader (Quality q) = renderHeader q.property <> ";q=" <> show q.value

-- | Manually construct a quality value.
quality :: forall a. a -> Number -> Either String (Quality a)
quality x n
    | n > 1.0 = Left "Invalid quality value, cannot be greater than 1"
    | n < 0.0 = Left "Invalid quality value, cannot be less than 0"
    | otherwise = case Global.readFloat <$> Global.toPrecision 3 n of 
        Nothing -> Left "Invalid precision."
        Just n'  -> Right $ Quality { property: x, value: n' }

-- | An opaque ordered representation of quality values without attached data.
newtype QualityOrder = QualityOrder Number

-- | Remove the attached data from a quality value, retaining only the
-- priority of the quality parameter.
qualityOrder :: forall a. Quality a -> QualityOrder
qualityOrder (Quality q) = QualityOrder $ q.value


-- | Attaches the quality value '1'.
maxQuality :: forall a. a -> Quality a
maxQuality property = Quality { property, value: 1.0 }

-- | Attaches the quality value '0'.
minQuality :: forall a. a -> Quality a
minQuality property = Quality { property, value: 0.0 }
