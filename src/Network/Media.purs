module Network.Media where

import Prelude

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Foldable (maximumBy, foldl)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..))
import Data.String (drop, lastIndexOf, split, splitAt, trim) as String
import Data.String.Utils (startsWith) as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Network.Media.Accept (class Accept, hasExtensionParameters, matches, parseAccept)
import Network.Media.Quality (Quality(..), maxQuality, mostSpecific, qualityOrder, readQ)
import Type.Proxy (Proxy(..))

matchAccept :: forall a.
    Accept a
    => Eq a
    => Array a -- ^ The server-side options
    -> String  -- ^ The client-side header value
    -> Maybe a
matchAccept options accepts = parseQuality accepts >>= matchQuality options

-- | Parses a full Accept header into a list of quality-valued media types.
parseQuality :: forall a. Accept a => String -> Maybe (Array (Quality a))
parseQuality = parseQuality' Proxy

parseQuality' :: forall a. Accept a => Proxy a -> String -> Maybe (Array (Quality a)) 
parseQuality' p str = for (String.split (Pattern ",") str) \s' -> do 
    let s = String.trim s'
        (Tuple accept q) = fromMaybe (Tuple s Nothing) $ if ext then findQ s else getQ s
    property <- parseAccept accept
    pure $ maybe (maxQuality property) (\value -> Quality { property, value })  q
    where 
        ext = hasExtensionParameters p

        -- Split on ';', and check if a quality value is there. A value of Nothing
        -- indicates there was no parameter, whereas a value of Nothing in the
        -- pair indicates the parameter was not a quality value.
        getQ x = do 
            msplit <- String.lastIndexOf (Pattern ";") x >>= (pure <<< flip String.splitAt x)
            let after = String.trim msplit.after 
                before = String.trim msplit.before 
            Just $ Tuple 
                    before
                    if String.startsWith ";q=" after then readQ $ String.drop 3 after else Nothing

        findQ s = do    
            let q = getQ s
            (Tuple a m) <- q
            maybe (findQ a) (const q) m

-- | Matches a list of server-side resource options against a pre-parsed
-- quality-marked list of client-side preferences. A result of 'Nothing' means
-- that nothing matched (which should indicate a 406 error). If two or more
-- results arise with the same quality level and specificity, then the first
-- one in the server list is chosen.
--
-- The use of the 'Accept' type class allows the application of either
-- 'MediaType' for the standard Accept header or String for any other
-- Accept header which can be marked with a quality value.
--
-- > matchQuality ["text/html", "application/json"] <$> parseQuality header
--
-- For more information on the matching process see RFC 2616, section 14.1-4.
matchQuality :: forall a. 
    Accept a
    => Eq a
    => Array a          -- ^ The server-side options
    -> Array (Quality a)  -- ^ The pre-parsed client-side header value
    -> Maybe a
matchQuality options acceptq = do 
    guard $ not (Array.null options)
    Quality q  <-  join $ maximumBy (compare `on` map qualityOrder) optionsq -- (\(Quality q) (Quality q2) -> compare q2.value q.value ) $ Array.mapMaybe (\x@(Quality q) -> if Array.elem q.property optionq then Just x else Nothing) acceptq
    guard $ q.value /= 0.0
    pure q.property
    where 
        optionsq = Array.reverse $ map addQuality options
        addQuality opt = withQValue opt <$> foldl (mfold opt) Nothing acceptq 
        withQValue opt (Quality qv) = Quality $ qv { property = opt }
        mfold opt cur acq@(Quality q)
            | opt `matches` q.property = mostSpecific acq <$> cur <|> Just acq
            | otherwise                = cur

        -- Quality m q <- maximumBy (compare `on` fmap qualityOrder) optionsq
        -- guard $ q /= 0
        -- return m
    -- where
    --     -- optionsq = reverse $ map addQuality options
    --     -- addQuality opt = withQValue opt <$> foldl' (mfold opt) Nothing acceptq
    --     -- withQValue opt qv = qv { qualityData = opt }
    --     mfold opt cur acq@(Quality q)
    --         | opt `matches` q.property = mostSpecific acq <$> cur <|> Just acq

