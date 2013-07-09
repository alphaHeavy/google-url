{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
  HasScheme(..),
  HasUrl(..),
  Hostname(..),
  InvalidUrl(..),
  Path(..),
  Port(..),
  Query(..),
  RelativeUrl(..),
  Scheme (..),
  Url(..)) where

import Bindings.Url
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Typeable
import Foreign.ForeignPtr

data FullyQualifiedUrl = FQU (ForeignPtr Gurl)
data RelativeUrl = RU (ForeignPtr Gurl)
data InvalidUrl = IU (ForeignPtr Gurl)
data FileUrl = FU (ForeignPtr Gurl)

data Url = FullyQualifiedUrl FullyQualifiedUrl | InvalidUrl InvalidUrl | FileUrl FileUrl

instance NFData FullyQualifiedUrl where
  rnf _ = ()

instance NFData RelativeUrl where
  rnf _ = ()

instance NFData InvalidUrl where
  rnf _ = ()

instance NFData FileUrl where
  rnf _ = ()

data Scheme = Http | Https | Mail deriving (Show,Eq,Ord)

data Username = Username Text deriving (Show,Eq,Ord)

data Password = Password Text deriving (Show,Eq,Ord)

data Hostname = Hostname Text deriving (Show,Eq,Ord,Typeable)

newtype Port = Port Int16 deriving (Enum,Eq,Integral,Num,Ord,Real,Show)

data Path = Path [Text] deriving (Show,Eq,Ord)

data Query = Query [(Text,Maybe Text)] deriving (Eq,Ord,Show)

data Fragment = Fragment Text deriving (Eq,Ord,Show)

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

instance NFData Hostname where
  rnf (Hostname x) = rnf x `seq` ()

instance NFData Url where
  rnf (FullyQualifiedUrl x) = rnf x `seq` ()
  rnf (InvalidUrl x) = rnf x `seq` ()
  rnf (FileUrl x) = rnf x `seq` ()
