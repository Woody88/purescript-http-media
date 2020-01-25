module Network.Media.MediaType.Internal where 

import Prelude

import Control.MonadZero (guard)
import Data.Array (foldMap, foldr)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested ((/\))
import Network.HTTP.Media.RenderHeader (class RenderHeader, renderHeader)
import Network.Media.Accept (class Accept, matches, parseAccept)
import Network.Media.Utils (mkCaseI)
import Unsafe.Coerce (unsafeCoerce)

-- | An HTTP media type, consisting of the type, subtype, and parameters.
newtype MediaType 
    = MediaType { mainType   :: CaseInsensitiveString   -- ^ The main type of the MediaType
                , subType    :: CaseInsensitiveString   -- ^ The sub type of the MediaType
                , parameters :: Parameters     -- ^ The parameters of the MediaType
                }

instance showMediaType :: Show MediaType where
    show = renderHeader

instance acceptMediaType :: Accept MediaType where 
    parseAccept str = do 
        ht    <- Array.uncons $ map String.trim $ String.split (Pattern ";") str 
        split <- (flip String.splitAt ht.head) <$> String.indexOf (Pattern "/") ht.head 
        guard $ (split.after /=  "" && split.before /= "*" || split.after /= "*" && split.before /= "")
        let parameters = foldr (\x acc -> insert (Array.uncons x) acc ) Map.empty $ map (String.split (Pattern "=")) ht.tail 
        pure $ MediaType { mainType: mkCaseI $ split.before 
                         , subType: mkCaseI $ String.drop 1 split.after
                         , parameters
                         }
        where 
            insert Nothing acc = acc
            insert (Just {head, tail}) acc = Map.insert (mkCaseI head) (mkCaseI $ foldMap identity tail) acc

    matches (MediaType a) (MediaType b) = case unit of 
        _ | b.mainType == mkCaseI "*" -> params
        _ | b.subType  == mkCaseI"*"  -> main 
          | otherwise               -> main && sub && params
        where
            main = a.mainType == b.mainType 
            sub = a.subType == b.subType 
            params = Map.isEmpty b.parameters || a.parameters == b.parameters 

instance renderHeaderMediatType :: RenderHeader MediaType where
    renderHeader (MediaType mt) =
        foldrWithIndex f (original mt.mainType <> "/" <> original mt.subType) mt.parameters
      where
        f k v acc = (acc <> (";" <> original k <> "=" <> original v))

-- | 'MediaType' parameters.
type Parameters = Map CaseInsensitiveString CaseInsensitiveString 

original :: CaseInsensitiveString -> String 
original = Newtype.unwrap