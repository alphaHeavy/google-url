module Data.Url (Url(..)) where

import Bindings.Url
import Data.ByteString

data Url where
  FullyQualifiedUrl :: ByteString -> Ptr Parsed -> Url

parseUrl :: Text -> Maybe Url
parseUrl str = undefined
