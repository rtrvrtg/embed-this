{-# LANGUAGE OverloadedStrings #-}

module Web.EmbedThis.DataParser.Twitter (
    TwitterDataParser(..)
) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.HashMap.Lazy
import Data.Maybe
import Data.Monoid
import Data.Text as T
import Data.Text (Text)
import Network.URI
import Web.EmbedThis.DataParser.Class

data TwitterDataParser = TwitterDataParser Value

instance DataParser TwitterDataParser where
    ingest = TwitterDataParser
    postIsValid = isJust . postURL

    postURL (TwitterDataParser (Object o)) = makeUri <$> parseMaybe parser o
      where
        parser o = do
            id <- o .: "id_str"
            user <- o .: "user"
            screen_name <- user .: "screen_name"
            return (id, screen_name)
        makeUri (a,b) = URI "https:" (Just $ URIAuth "" "twitter.com" "") ("/" <> a <> "/status/" <> b) "" ""
    postURL _ = fail "Bad object format"

    postText (TwitterDataParser (Object o)) = parseMaybe (\o -> o .: "text") o
    postText _ = fail "Bad object format"

    postImages _ = fail "unimplemented"

    postLink (TwitterDataParser (Object o)) = makeLink <$> parseMaybe parser o
      where
        parser o = do
            created_at <- o .: "created_at"
            return (postURL (TwitterDataParser (Object o)), created_at)
        makeLink (a,b) = "<a href=\"" <> (T.pack $ show a) <> "\">" <> b <> "</a>"
    postLink _ = fail "unimplemented"

    postUserLink (TwitterDataParser (Object o)) = makeLink <$> parseMaybe parser o
      where
        parser o = do
            user <- o .: "user"
            screen_name <- user .: "screen_name"
            name <- user .: "name"
            return (screen_name, name)
        makeLink (a,b) = "<a href=\"" <> "https://twitter.com/" <> a <> "\">" <> b <> "</a>"
    postUserLink _ = fail "unimplemented"

    postIntents (TwitterDataParser _) = Just ["tweet", "retweet", "favorite"]

    postIntent _ _ _ = fail "unimplemented"
