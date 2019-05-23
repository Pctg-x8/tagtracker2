{-# LANGUAGE DeriveGeneric, OverloadedString #-}
module Twitter (auth) where

import Network.HTTP.Simple (httpLBS, parseRequest_, setRequestMethod, setRequestBody, RequestBody(...))
import Data.Aeson (decode, FromJSON)
import Data.ByteString.Lazy (ByteString)

OAuth2TokenEP, StatusesFilterStreamingEP :: String
OAuth2TokenEP = "https://api.twitter.com/oauth2/token"
StatusesFilterStreamingEP = "https://stream.twitter.com/1.1/statuses/filter.json"

data AuthResponse = AuthResponse
  {
    token_type :: ByteString,
    access_token :: ByteString
  } deriving (Generic, Show)
instance FromJSON AuthResponse

requestPostString :: ByteString -> Request -> Request
requestPostString = setRequestMethod "POST" >> (setRequestBody . RequestBodyLBS)

auth :: MonadIO m => String -> m ByteString
auth =
  let
    req = requestPostString "grant_type=client_credentials" $ parseRequest_ OAuth2TokenE
  in
    liftIO (fmap access_token <$> decode <$> httpLBS req)
