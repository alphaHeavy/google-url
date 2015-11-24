{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Url.Types (
  FileUrl(..),
  Fragment(..),
  FullyQualifiedUrl(..),
  HasHostname(..),
  HasPath(..),
  HasPort(..),
  HasQuery(..),
  HasFragment(..),
  HasPassword(..),
  HasScheme(..),
  HasUrl(..),
  HasUsername(..),
  Hostname(..),
  InvalidUrl(..),
  Path(..),
  Port(..),
  Query(..),
  RelativeUrl(..),
  Scheme (..),
  UnknownUrl (..),
  Url(..)) where

import Bindings.Url
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Int
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Foreign.ForeignPtr

data FullyQualifiedUrl = FQU (ForeignPtr Gurl)
data RelativeUrl = RU (ForeignPtr Gurl)
data InvalidUrl = IU (ForeignPtr Gurl)
data FileUrl = FU (ForeignPtr Gurl)
data UnknownUrl = UU (ForeignPtr Gurl)

data Url = FullyQualifiedUrl FullyQualifiedUrl | InvalidUrl InvalidUrl | FileUrl FileUrl | UnknownUrl UnknownUrl

instance NFData FullyQualifiedUrl where
  rnf _ = ()

instance NFData RelativeUrl where
  rnf _ = ()

instance NFData InvalidUrl where
  rnf _ = ()

instance NFData FileUrl where
  rnf _ = ()

data Scheme = Http | Https | Mail | File | Unknown Text deriving (Show,Eq,Ord)

newtype Username = Username Text deriving (Show,Eq,Ord)

newtype Password = Password Text deriving (Show,Eq,Ord)

newtype Hostname = Hostname Text deriving (Show,Eq,Ord,Typeable)

instance IsString Hostname where
  fromString = Hostname . T.pack

newtype Port = Port Int16 deriving (Enum,Eq,Integral,Num,Ord,Real,Show)

newtype Path = Path [Text] deriving (Show,Eq,Ord)

newtype Query = Query [(Text,Maybe Text)] deriving (Eq,Ord,Show)

newtype Fragment = Fragment Text deriving (Eq,Ord,Show)

class HasUrl a where
  toText :: a -> Text
  isStandard :: a -> Bool
  isValid :: a -> Bool

class HasScheme a where
  getScheme :: a -> Scheme
  hasScheme :: a -> Bool
  setScheme :: a -> Scheme -> a

class HasHostname a where
  getHostname :: a -> Hostname
  hasHostname :: a -> Bool

class HasPort a where
  getPort :: a -> Port
  getEffectivePort :: a -> Port
  hasPort :: a -> Bool
  setPort :: a -> Port -> a

class HasPath a where
  getPath :: a -> Path
  getPathForRequest :: a -> ByteString

class HasQuery a where
  getQuery :: a -> Maybe Query
  getQueryForRequest :: a -> ByteString

class HasFragment a where
  getFragment :: a -> Maybe Fragment
  getFragmentForRequest :: a -> ByteString

class HasUsername a where
  hasUsername :: a -> Bool
  getUsername :: a -> Maybe ByteString

class HasPassword a where
  hasPassword :: a -> Bool
  getPassword :: a -> Maybe ByteString

instance NFData Hostname where
  rnf (Hostname x) = rnf x `seq` ()

instance NFData Url where
  rnf (FullyQualifiedUrl x) = rnf x `seq` ()
  rnf (InvalidUrl x) = rnf x `seq` ()
  rnf (FileUrl x) = rnf x `seq` ()
