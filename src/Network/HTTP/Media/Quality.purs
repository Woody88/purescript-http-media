module Network.HTTP.Media.Quality where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Number (fromString) as Number
import Data.Number.Format (fixed, toStringWith) as Number
import Network.HTTP.Media.RenderHeader (class RenderHeader, renderHeader)
import Network.HTTP.Media.Accept (class Accept, moreSpecificThan)

-- | Attaches a quality value to data.
newtype Quality a = Quality { property :: a, value :: Number } 

derive instance newtypeQuality :: Newtype (Quality a) _
derive instance functorQuality :: Functor Quality 

instance showQuality :: Show a => Show (Quality a) where 
    show (Quality q) = "(Quality " <> show q.property <> " " <> show q.value <> ")"

instance renderHeaderQuality :: RenderHeader h => RenderHeader (Quality h) where
    renderHeader (Quality q) = renderHeader q.property <> ";q=" <> show q.value

-- | Manually construct a quality value.
quality :: forall a. a -> Number -> Either String (Quality a)
quality x n
    | n > 1.0 = Left "Invalid quality value, cannot be greater than 1"
    | n < 0.0 = Left "Invalid quality value, cannot be less than 0"
    | otherwise = (\n' -> Quality { property: x, value: n' }) <$> readQ' n 

-- | An opaque ordered representation of quality values without attached data.
newtype QualityOrder = QualityOrder Number

derive instance eqQualityOrder :: Eq QualityOrder 
derive instance ordQualityOrder :: Ord QualityOrder 

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

-- | Combines quality values by specificity. Selects the more specific of the
-- two arguments, but if they are the same returns the data of the left
-- argument with the two quality values of both arguments combined.
mostSpecific :: forall a. Accept a => Quality a -> Quality a -> Quality a
mostSpecific (Quality q1) (Quality q2)
    | q1.property `moreSpecificThan` q2.property = wrap q1
    | q2.property `moreSpecificThan` q1.property = wrap q2
    | otherwise             = wrap $ q1 { value= q' }
  where
    q' = div (q1.value *  q2.value) 1.0

readQ :: String -> Maybe Number 
readQ str = Number.fromString str >>= (hush <<< readQ')

readQ' :: Number -> Either String Number 
readQ' n
    | n > 1.0 = Left "Invalid quality value, cannot be greater than 1"
    | n < 0.0 = Left "Invalid quality value, cannot be less than 0"
    | otherwise = case Number.fromString $ Number.toStringWith (Number.fixed 3) n of 
        Nothing -> Left "Invalid precision"
        Just n' -> Right n'
    
    