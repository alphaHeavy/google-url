{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Url (
  parseUrl,
  parseFullyQualifiedUrl,
  resolveRelativeUrl,
  FileUrl(..),
  FullyQualifiedUrl(..),
  HasHostname(..),
  HasPath(..),
  HasPort(..),
  HasQuery(..),
  HasFragment(..),
  HasScheme(..),
  InvalidUrl(..),
  RelativeUrl(..),
  Url(..),
  module Data.Url.Types) where

import Control.Exception (mask_)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Url.Internal as I
import Data.Url.Types
import Foreign.ForeignPtr
import System.IO.Unsafe


instance HasScheme Url where
  hasScheme (FullyQualifiedUrl _) = True
  hasScheme (InvalidUrl (IU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.hasScheme val 
  hasScheme (FileUrl (FU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.hasScheme val

  getScheme (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getScheme val
  getScheme (InvalidUrl (IU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getScheme val
  getScheme (FileUrl (FU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getScheme val


  setScheme (FullyQualifiedUrl (FQU gurl)) scheme = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    result <- I.setScheme val scheme
    foreignPtr <- newForeignPtr I.p'freeUrl result
    return $ FullyQualifiedUrl $ FQU foreignPtr
  setScheme (InvalidUrl (IU gurl)) scheme = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    result <- I.setScheme val scheme
    foreignPtr <- newForeignPtr I.p'freeUrl result
    return $ InvalidUrl $ IU foreignPtr
  setScheme (FileUrl (FU gurl)) scheme = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    result <- I.setScheme val scheme
    foreignPtr <- newForeignPtr I.p'freeUrl result
    return $ FileUrl $ FU foreignPtr


instance HasUrl FullyQualifiedUrl where
  toText (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val 
  isStandard (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val
  isValid (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isValid val


instance HasUrl Url where
  toText (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val
  toText (InvalidUrl (IU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val
  toText (FileUrl (FU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val
  isStandard (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val
  isStandard (InvalidUrl (IU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val
  isStandard (FileUrl (FU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val
  isValid (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isValid val
  isValid (InvalidUrl (IU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isValid val
  isValid (FileUrl (FU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isValid val
  

instance HasPort Url where
  hasPort (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.hasPort val
  hasPort (InvalidUrl (IU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.hasPort val
  hasPort (FileUrl _) = False

  getPort (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getPort val
  getPort (InvalidUrl (IU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getPort val

  getEffectivePort (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getEffectivePort val
  setPort (FullyQualifiedUrl (FQU gurl)) port = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    result <- I.setPort val port
    foreignPtr <- newForeignPtr I.p'freeUrl result
    return $ FullyQualifiedUrl $ FQU foreignPtr


instance HasPath Url where
  getPath (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getPath val
  getPathForRequest (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getPathForRequest val

instance HasHostname Url where
  getHostname (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getHostname val
  hasHostname (FullyQualifiedUrl _) = True

instance HasScheme FullyQualifiedUrl where
  hasScheme _ = True
  getScheme (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getScheme val
  setScheme (FQU gurl) scheme = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    result <- I.setScheme val scheme
    foreignPtr <- newForeignPtr I.p'freeUrl result
    return $ FQU foreignPtr 

instance HasHostname FullyQualifiedUrl where
  getHostname (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getHostname val
  hasHostname (FQU _) = True

instance HasPort FullyQualifiedUrl where
  hasPort _ = True
  getPort (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getPort val
  getEffectivePort (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getEffectivePort val
  setPort (FQU gurl) port = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    result <- I.setPort val port
    foreignPtr <- newForeignPtr I.p'freeUrl result
    return $ FQU foreignPtr

instance HasPath FullyQualifiedUrl where
  getPath (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getPath val
  getPathForRequest (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getPathRaw val

instance HasQuery FullyQualifiedUrl where
  getQuery (FQU gurl) = 
    let x@(Query y) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getQuery val
    in if L.length y > 0 then Just x else Nothing
  getQueryForRequest (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getQueryRaw val

instance HasFragment FullyQualifiedUrl where
  getFragment (FQU gurl) = 
    let x@(Fragment y) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getFragment val
    in if T.null y then Nothing else Just x
  getFragmentForRequest (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getFragmentRaw val


instance Eq Url where
  (FullyQualifiedUrl (FQU url1)) == (FullyQualifiedUrl (FQU url2)) = unsafePerformIO $ mask_ $ withForeignPtr url1 $ \ val1 -> withForeignPtr url2 $ \ val2 -> I.equals val1 val2
  (InvalidUrl (IU url1)) == (InvalidUrl (IU url2)) = unsafePerformIO $ mask_ $ withForeignPtr url1 $ \ val1 -> withForeignPtr url2 $ \ val2 -> I.equals val1 val2
  (FileUrl (FU url1)) == (FileUrl (FU url2)) = unsafePerformIO $ mask_ $ withForeignPtr url1 $ \ val1 -> withForeignPtr url2 $ \ val2 -> I.equals val1 val2
  _ == _ = False

instance Show Url where
  show = T.unpack . toText

instance Show FullyQualifiedUrl where
  show (FQU gurl) = T.unpack $ unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val

instance Show RelativeUrl where
  show (RU gurl) = T.unpack $ unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val

instance Show InvalidUrl where
  show (IU gurl) = T.unpack $ unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val

instance Show FileUrl where
  show (FU gurl) = T.unpack $ unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val

parseUrl :: Text -> Url
parseUrl str = 
  unsafePerformIO $ 
    mask_ $ do
      parsed <- I.parseUrl str
      foreignPtr <- newForeignPtr I.p'freeUrl parsed
      valid <- I.isValid parsed
      case valid of
        False -> return $ InvalidUrl $ IU foreignPtr
        True -> do
          standard <- I.isStandard  parsed
          case standard of
            False -> undefined
            True -> return $ FullyQualifiedUrl $ FQU foreignPtr

{- This function is unsafe. It assumes that the string is fully qualified url. Use with caution. -}
parseFullyQualifiedUrl :: Text -> FullyQualifiedUrl
parseFullyQualifiedUrl str =
  unsafePerformIO $ 
    mask_ $ do
      parsed <- I.parseUrl str
      foreignPtr <- newForeignPtr I.p'freeUrl parsed
      return $ FQU foreignPtr

resolveRelativeUrl :: FullyQualifiedUrl -> Text -> FullyQualifiedUrl
resolveRelativeUrl (FQU gurl) relativeUrl =
  unsafePerformIO $ mask_ $ 
    withForeignPtr gurl $ \ fullUrl -> do
      result <- I.resolve relativeUrl fullUrl
      foreignPtr <- newForeignPtr I.p'freeUrl result
      return $ FQU foreignPtr
