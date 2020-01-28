module Network.HTTP.Media.MediaType.Internal where 

import Prelude

import Control.MonadZero (guard)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Network.HTTP.Media.RenderHeader (class RenderHeader, renderHeader)
import Network.Media.Accept (class Accept, matches)
import Network.Media.Utils (mkCaseI)

-- | An HTTP media type, consisting of the type, subtype, and parameters.
newtype MediaType 
    = MediaType { mainType   :: CaseInsensitiveString   -- ^ The main type of the MediaType
                , subType    :: CaseInsensitiveString   -- ^ The sub type of the MediaType
                , parameters :: Parameters     -- ^ The parameters of the MediaType
                }

derive instance newtypeMediaType :: Newtype MediaType _
derive instance eqMediaType :: Eq MediaType 

instance showMediaType :: Show MediaType where
    show = renderHeader

instance acceptMediaType :: Accept MediaType where 
    parseAccept str = do 
        ht    <- Array.uncons $ map String.trim $ String.split (Pattern ";") str 
        split <- (flip String.splitAt ht.head) <$> String.indexOf (Pattern "/") ht.head 
        guard $ (split.after /=  "" && split.before /= "*" || split.after /= "*" && split.before /= "") 
        let 
            x = map (\y -> flip String.splitAt y <$> String.indexOf (Pattern "=") y) ht.tail 
        parameters <- foldM (\acc mx -> insert mx acc) Map.empty x
        pure $ MediaType { mainType: mkCaseI $ split.before 
                         , subType: mkCaseI $ String.drop 1 split.after
                         , parameters
                         }
        where 
            insert Nothing acc = Nothing
            insert (Just {before, after}) acc = Just $ Map.insert (mkCaseI before) (mkCaseI $ String.drop 1 after) acc

    matches (MediaType a) (MediaType b) = case unit of 
        _ | b.mainType == mkCaseI "*" -> params
        _ | b.subType  == mkCaseI"*"  -> main 
          | otherwise               -> main && sub && params
        where
            main = a.mainType == b.mainType 
            sub = a.subType == b.subType 
            params = Map.isEmpty b.parameters || a.parameters == b.parameters 

    moreSpecificThan a b = (a `matches` b) &&
        ((unwrap a).mainType == mkCaseI "*" && anyB && params ||
        (unwrap a).subType == mkCaseI "*" && (anyB || subB && params) ||
        anyB || subB || params)
        where
            anyB = (unwrap b).mainType == mkCaseI "*"
            subB = (unwrap b).subType == mkCaseI "*"
            params = not (Map.isEmpty $ (unwrap a).parameters) && Map.isEmpty (unwrap b).parameters

    hasExtensionParameters _ = true

instance renderHeaderMediatType :: RenderHeader MediaType where
    renderHeader (MediaType mt) =
        foldrWithIndex f (original mt.mainType <> "/" <> original mt.subType) mt.parameters
      where
        f k v acc = (acc <> (";" <> original k <> "=" <> original v))

-- | 'MediaType' parameters.
type Parameters = Map CaseInsensitiveString CaseInsensitiveString 

original :: CaseInsensitiveString -> String 
original = unwrap