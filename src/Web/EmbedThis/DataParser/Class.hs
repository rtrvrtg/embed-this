{-# LANGUAGE OverloadedStrings #-}

module Web.EmbedThis.DataParser.Class (
    DataParser(..)
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

class DataParser a where
    ingest       :: Value -> a
    postIsValid  :: a -> Bool
    postURL      :: a -> Maybe URI
    postText     :: a -> Maybe Text
    postImages   :: a -> Maybe [URI]
    postLink     :: a -> Maybe Text
    postUserLink :: a -> Maybe Text
    postIntents  :: a -> Maybe [Text]
    postIntent   :: a -> Text -> HashMap Text Text -> Maybe Text
