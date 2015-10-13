{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.EmbedThis.UrlParser (
    UrlAction(..),
    UrlParser(..),
    TwitterUrlParser(..)
) where

import Data.ByteString (ByteString)
import Data.List as L
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding as E
import Data.Text (Text)
import Network.HTTP.Types.URI (Query, parseQuery)
import Network.URI
import Safe (headMay)

data UrlAction = FetchOne Text (Maybe Text)
               | FetchMany Text (Maybe Text)

class UrlParser a where
    parseUrl :: a -> URI -> Maybe UrlAction

data TwitterUrlParser = TwitterUrlParser

instance UrlParser TwitterUrlParser where
    parseUrl _ URI{..} = if (domainMatch uriAuthority)
        then (pp $ T.pack uriPath)
        else Nothing
      where
        domainMatch (Just a) = or $ [
            uriRegName a == "twitter.com",
            T.isSuffixOf ".twitter.com" (T.pack $ uriRegName a)]
        domainMatch _ = False
        pp :: Text -> Maybe UrlAction
        pp p = case L.tail $ T.splitOn "/" p of
            ("search" : _) ->
                searchQuery (T.pack uriQuery)
            ("1.1" : "search" : "tweets.json" : _) ->
                searchQuery (T.pack uriQuery)
            ("statuses" : "show" : i : _) ->
                itemQuery i Nothing
            (un : "status" : i : _) ->
                itemQuery i (Just un)
            _ -> Nothing
        itemQuery :: Text -> Maybe Text -> Maybe UrlAction
        itemQuery a b = Just $ FetchOne a b
        searchQuery :: Text -> Maybe UrlAction
        searchQuery q = flip FetchMany Nothing <$> (getQueryStringVar "q" q)

getQueryStringVar :: Text -> Text -> Maybe Text
getQueryStringVar key query = E.decodeUtf8 <$> possibleValue
  where
    possibleValue :: Maybe ByteString
    possibleValue = headList . tryGetKey $ query
    headList :: [(ByteString, Maybe ByteString)] -> Maybe ByteString
    headList = headMay . catMaybes . fmap snd
    tryGetKey :: Text -> [(ByteString, Maybe ByteString)]
    tryGetKey = L.filter isItem . parseQuery . E.encodeUtf8
    isItem :: (ByteString, Maybe ByteString) -> Bool
    isItem (x,Just y) = x == (E.encodeUtf8 key)
    isItem (x,_) = False
