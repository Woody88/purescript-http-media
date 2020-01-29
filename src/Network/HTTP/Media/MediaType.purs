module Network.HTTP.Media.MediaType 
    ( module MediaType
    , (//)
    , (/:)
    , (/?)
    , (/.)
    , mkMediaType
    , mediaTypeWithParam
    , mediaTypeHasParams
    , mediaTypeLookupParam
    , mainType
    , subType
    , parameters
    )
    where

import Prelude

import Data.Array (foldr)
import Data.Array as Array
import Data.Char.Unicode (isDigit, isLetter)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Network.HTTP.Media.MediaType.Internal (MediaType(..), Parameters)
import Network.HTTP.Media.MediaType.Internal (MediaType(..), Parameters) as MediaType
import Network.HTTP.Media.Utils (mkCaseI)

infixl 7 mkMediaType as //
infixl 7 mediaTypeWithParam as /:
infixl 7 mediaTypeHasParams as /?
infixl 7 mediaTypeLookupParam as /.

-- | Retrieves the main type of a 'MediaType'.
mainType :: MediaType -> CaseInsensitiveString 
mainType (MediaType x) = x.mainType

-- | Retrieves the sub type of a 'MediaType'.
subType :: MediaType -> CaseInsensitiveString
subType (MediaType x) = x.subType

-- | Retrieves the parameters of a 'MediaType'.
parameters :: MediaType -> Parameters
parameters (MediaType x) = x.parameters

-- | List of the valid characters for a media-type `reg-name` as per RFC 4288.
mediaChars :: Char -> Boolean 
mediaChars x = isLetter x || isDigit x || isSymbol x
    where 
        isSymbol = flip Array.elem ['!', '#', '$', '&', '.', '+', '-', '^', '_']

-- | Evaluates whether the given character is valid in a media type `reg-name`as per RFC 4288.
isMediaChar :: Char -> Boolean
isMediaChar = mediaChars

mkMediaType :: String -> String -> MediaType    
mkMediaType a b 
    | a == "*" && b == "*" = MediaType { mainType: mkCaseI a, subType: mkCaseI b, parameters: mempty }
    | b == "*"             = MediaType  { mainType: ensureR a, subType: mkCaseI b, parameters: mempty }
    | otherwise            = MediaType { mainType: ensureR a, subType: ensureR b, parameters: mempty }

-- | Adds a parameter to a 'MediaType'. Can produce an error if either
-- string is invalid.
mediaTypeWithParam :: MediaType -> Tuple String String -> MediaType 
mediaTypeWithParam (MediaType mt) (Tuple k v) = 
    MediaType 
        { mainType: mt.mainType
        , subType: mt.subType
        , parameters: Map.insert (ensureR k)  (ensureV v)  mt.parameters 
        }

-- | Evaluates if a 'MediaType' has a parameter of the given name.
mediaTypeHasParams :: MediaType -> String -> Boolean
mediaTypeHasParams (MediaType mt) k = Map.member (mkCaseI k) mt.parameters

-- | Retrieves a parameter from a 'MediaType'.
mediaTypeLookupParam :: MediaType -> String -> Maybe CaseInsensitiveString
mediaTypeLookupParam (MediaType mt) k = Map.lookup (mkCaseI k) mt.parameters

-- | Ensures that the 'ByteString' matches the ABNF for `reg-name` in RFC 4288.
ensureR :: String -> CaseInsensitiveString
ensureR "" = unsafeThrow "Invalid length, must not be empty"
ensureR str = mkCaseI $ ensure isMediaChar str

-- | Ensures that the 'ByteString' does not contain invalid characters for
-- a parameter value. RFC 4288 does not specify what characters are valid, so
-- here we just disallow parameter and media type breakers, ',' and ';'.
ensureV :: String -> CaseInsensitiveString
ensureV str = mkCaseI $ ensure (flip Array.notElem [',', ';']) str

-- | Ensures the predicate matches for every character in the given string.
ensure :: (Char -> Boolean) -> String -> String
ensure f str = 
    if foldr (\a b -> a && b) true $ map isMediaChar $ String.toCharArray str
    then str 
    else unsafeThrow $ "Invalid character in " <> str