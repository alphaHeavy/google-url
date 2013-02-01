{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Url.Types (
  HasHostname(..),
  HasPath(..),
  HasPort(..),
  HasQuery(..),
  HasFragment(..),
  HasScheme(..),
  Hostname(..),
  ParsedUrl(..),
  Scheme (..),
  Url(..),
  UrlKind(..),
  ValidUrl) where

import Bindings.Url
import Data.Text (Text)
import Foreign.ForeignPtr

data UrlKind = Full | Relative | Invalid | Other

type family ValidUrl (x :: UrlKind) :: Bool

type instance ValidUrl x = ValidUrlOf x ['Full,'Relative]

type family ValidUrlOf (x :: UrlKind) (xs :: [UrlKind]) :: Bool

type instance ValidUrlOf x xs = 'True

data Url (a :: UrlKind) where
  FullyQualifiedUrl :: ForeignPtr Gurl -> Url 'Full
  RelativeUrl :: ForeignPtr Gurl -> Url 'Relative
  InvalidUrl :: ForeignPtr Gurl -> Url 'Invalid
  FileUrl :: ForeignPtr Gurl -> Url 'Other
  CanonicalUrl :: ForeignPtr Gurl -> Url 'Full

data ParsedUrl = ParsedFullyQualifiedUrl (Url 'Full) | ParsedRelativeUrl (Url 'Relative) | ParsedInvalidUrl (Url 'Invalid)

--unsafeParseCanonical :: ByteString -> Url 'Full

--instance Eq Url where
  

data Scheme = Http | Https | Mail deriving (Show,Eq)

data Username = Username Text deriving (Show,Eq)

data Password = Password Text deriving (Show,Eq)

data Hostname = Hostname Text deriving (Show,Eq,Ord)

data Port = Port Int deriving (Show,Eq,Ord)

data Path = Path [Text] deriving (Show,Eq,Ord)

data Query = Query [(Text,Maybe Text)] deriving (Eq,Ord,Show)

data Fragment = Fragment Text deriving (Eq,Ord,Show)

class HasScheme a where
  getScheme :: a -> Scheme
  hasScheme :: a -> Bool

class HasHostname a where
  getHostname :: a -> Hostname
  hasHostname :: a -> Bool

class HasPort a where
  getPort :: a -> Port

class HasPath a where
  getPath :: a -> Path

class HasQuery a where
  getQuery :: a -> Maybe Query

class HasFragment a where
  getFragment :: a -> Maybe Fragment

