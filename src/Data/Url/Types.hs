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
  Scheme (..),
  Url(..)) where

import Bindings.Url
import Data.Text (Text)
import Foreign.ForeignPtr

data Url where
  FullyQualifiedUrl :: ForeignPtr Gurl -> Url
  RelativeUrl :: ForeignPtr Gurl -> Url
  InvalidUrl :: ForeignPtr Gurl -> Url
  FileUrl :: ForeignPtr Gurl -> Url

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

