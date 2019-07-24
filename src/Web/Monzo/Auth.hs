{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Monzo.Auth where

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import GHC.Generics

import Web.Monzo

import           Data.Text (Text)

import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8

import Data.String

import Data.Aeson

-- | Construct the URI the user needs to visit to retrieve an authorization code.
authUri :: (IsString s, Monoid s) => s -> s -> s -> s
authUri clientId redirectUri stateToken = mconcat 
        [ authbase
        , "?client_id=", clientId
        , "&redirect_uri=", redirectUri
        , "&response_type=code"
        , "&state=", stateToken
        ]
        where
            authbase = "https://auth.monzo.com"


-- | Exchange the authorization code for an access token
exchange :: Manager -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Either String AccessTokenResponse)
exchange mgr clientId clientSecret redirectUri code = do
    req <- parseRequest (base <> "/oauth2/token")
    eitherDecode' . responseBody <$> flip httpLbs mgr (urlEncodedBody body req)
    where
        body = 
            [ ("grant_type", "authorization_code")
            , ("client_id",  clientId)
            , ("client_secret", clientSecret)
            , ("redirect_uri", redirectUri)
            , ("code", code)
            ]

-- | Response from API on successful access token exchange.
data AccessTokenResponse = AccessTokenResponse
    { atrAccessToken  :: Text
    , atrClientId     :: Text
    , atrExpiresIn    :: Integer
    , atrRefreshToken :: Text
    , atrTokenType    :: Text
    , atrUserId       :: Text
    }
    deriving (Generic, Show)

instance FromJSON AccessTokenResponse where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 3
        }
