module Network.Media.MediaType 
    ( module MediaType
    , (//)
    , (/:)
    , mkMediaType
    , mediaTypeWithParam
    , mainType
    , subType
    , parameters
    )
    where

import Prelude

import Data.Array (foldr)
import Data.Array as Array
import Data.Char.Unicode (isDigit, isLetter)
import Data.Either (Either(..))
import Data.Map as Map
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Network.Media.MediaType.Internal (MediaType(..), Parameters)
import Network.Media.MediaType.Internal (MediaType(..), Parameters) as MediaType
import Network.Media.Utils (mkCaseI)

infixl 7 mkMediaType as //
infixl 7 mediaTypeWithParam as /:

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

mkMediaType :: String -> String -> Either String MediaType    
mkMediaType a b 
    | a == "*" && b == "*" = Right $ MediaType { mainType: mkCaseI a, subType: mkCaseI b, parameters: mempty }
    | b == "*"             = ensureR a >>= \mainType -> Right $ MediaType  { mainType, subType: mkCaseI b, parameters: mempty }
    | otherwise            = ensureR a >>= \mainType -> ensureR b >>= \subType -> Right $ MediaType { mainType, subType, parameters: mempty }

-- | Adds a parameter to a 'MediaType'. Can produce an error if either
-- string is invalid.
mediaTypeWithParam :: Either String MediaType -> Tuple String String -> Either String MediaType 
mediaTypeWithParam (Left e) _ = Left e 
mediaTypeWithParam (Right (MediaType mt)) (Tuple k v) = do 
    key   <- ensureR k 
    value <- ensureV v 
    pure $ MediaType { mainType: mt.mainType
                     , subType: mt.subType
                     , parameters: Map.insert key value mt.parameters 
                     }

-- | Ensures that the 'ByteString' matches the ABNF for `reg-name` in RFC 4288.
ensureR :: String -> Either String CaseInsensitiveString
ensureR "" = Left "Invalid length, must not be empty"
ensureR str = mkCaseI <$> ensure isMediaChar str

-- | Ensures that the 'ByteString' does not contain invalid characters for
-- a parameter value. RFC 4288 does not specify what characters are valid, so
-- here we just disallow parameter and media type breakers, ',' and ';'.
ensureV :: String -> Either String CaseInsensitiveString
ensureV str = mkCaseI <$> ensure (flip Array.notElem [',', ';']) str

-- | Ensures the predicate matches for every character in the given string.
ensure :: (Char -> Boolean) -> String -> Either String String
ensure f str = 
    if foldr (\a b -> a && b) true $ map isMediaChar $ String.toCharArray str
    then Right str 
    else Left $ "Invalid character in " <> str