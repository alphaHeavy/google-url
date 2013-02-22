{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Url.Types (
  FileUrl(..),
  FullyQualifiedUrl(..),
  HasHostname(..),
  HasPath(..),
  HasPort(..),
  HasQuery(..),
  HasFragment(..),
  HasScheme(..),
  HasUrl(..),
  Hostname(..),
  InvalidUrl(..),
  RelativeUrl(..),
  Scheme (..),
  Url(..)) where

import Bindings.Url
import Control.DeepSeq
import Data.Text (Text)
import Foreign.ForeignPtr

data FullyQualifiedUrl = FQU (ForeignPtr Gurl)
data RelativeUrl = RU (ForeignPtr Gurl)
data InvalidUrl = IU (ForeignPtr Gurl)
data FileUrl = FU (ForeignPtr Gurl)

data Url = FullyQualifiedUrl FullyQualifiedUrl | RelativeUrl RelativeUrl | InvalidUrl InvalidUrl | FileUrl FileUrl

instance NFData FullyQualifiedUrl where
  rnf _ = ()

instance NFData RelativeUrl where
  rnf _ = ()

instance NFData InvalidUrl where
  rnf _ = ()

instance NFData FileUrl where
  rnf _ = ()


{-
data Url where
  FullyQualifiedUrl :: ForeignPtr Gurl -> Url
  RelativeUrl :: ForeignPtr Gurl -> Url
  InvalidUrl :: ForeignPtr Gurl -> Url
  FileUrl :: ForeignPtr Gurl -> Url

--unsafeParseCanonical :: ByteString -> Url 'Full

--instance Eq Url where
-}

data Scheme = Http | Https | Mail deriving (Show,Eq,Ord)

data Username = Username Text deriving (Show,Eq)

data Password = Password Text deriving (Show,Eq)

data Hostname = Hostname Text deriving (Show,Eq,Ord)

data Port = Port Int deriving (Show,Eq,Ord)

data Path = Path [Text] deriving (Show,Eq,Ord)

data Query = Query [(Text,Maybe Text)] deriving (Eq,Ord,Show)

data Fragment = Fragment Text deriving (Eq,Ord,Show)

class HasUrl a where
  toText :: a -> Text
  isStandard :: a -> Bool

class HasScheme a where
  getScheme :: a -> Scheme
  hasScheme :: a -> Bool
  setScheme :: a -> Scheme -> a

class HasHostname a where
  getHostname :: a -> Hostname
  hasHostname :: a -> Bool

class HasPort a where
  getPort :: a -> Port
  hasPort :: a -> Bool

class HasPath a where
  getPath :: a -> Path

class HasQuery a where
  getQuery :: a -> Maybe Query

class HasFragment a where
  getFragment :: a -> Maybe Fragment

instance NFData Hostname where
  rnf (Hostname x) = rnf x `seq` ()
