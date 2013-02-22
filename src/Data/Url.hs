{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Url.Internal as I
import Data.Url.Types
import Foreign.ForeignPtr
import System.IO.Unsafe


instance HasScheme Url where
  hasScheme (FullyQualifiedUrl _) = True
  getScheme (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getScheme val
  setScheme (FullyQualifiedUrl (FQU gurl)) scheme = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    result <- I.setScheme val scheme
    foreignPtr <- newForeignPtr I.p'freeUrl result
    return $ FullyQualifiedUrl $ FQU foreignPtr

instance HasUrl FullyQualifiedUrl where
  toText (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val 
  isStandard (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val

instance HasUrl Url where
  toText (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val
  toText (RelativeUrl (RU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val
  toText (InvalidUrl (IU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val
  toText (FileUrl (FU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val
  isStandard (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val
  isStandard (RelativeUrl (RU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val
  isStandard (InvalidUrl (IU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val
  isStandard (FileUrl (FU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val

instance HasHostname Url where
  getHostname (FullyQualifiedUrl (FQU gurl)) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getHostname val
  hasHostname (FullyQualifiedUrl _) = True

instance HasHostname FullyQualifiedUrl where
  getHostname (FQU gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getHostname val
  hasHostname (FQU _) = True

instance Eq Url where
  (FullyQualifiedUrl (FQU url1)) == (FullyQualifiedUrl (FQU url2)) = unsafePerformIO $ mask_ $ withForeignPtr url1 $ \ val1 -> withForeignPtr url2 $ \ val2 -> I.equals val1 val2
  (RelativeUrl (RU url1)) == (RelativeUrl (RU url2)) = unsafePerformIO $ mask_ $ withForeignPtr url1 $ \ val1 -> withForeignPtr url2 $ \ val2 -> I.equals val1 val2
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
            False -> return $ RelativeUrl $ RU foreignPtr
            True -> return $ FullyQualifiedUrl $ FQU foreignPtr

{- This function is unsafe. It assumes that the string is fully qualified url. Use with caution. -}
parseFullyQualifiedUrl :: Text -> FullyQualifiedUrl
parseFullyQualifiedUrl str =
  unsafePerformIO $ 
    mask_ $ do
      parsed <- I.parseUrl str
      foreignPtr <- newForeignPtr I.p'freeUrl parsed
      return $ FQU foreignPtr

resolveRelativeUrl :: FullyQualifiedUrl -> RelativeUrl -> FullyQualifiedUrl
resolveRelativeUrl (FQU gurl) (RU relativeUrl) =
  unsafePerformIO $ mask_ $ 
    withForeignPtr gurl $ \ fullUrl ->
      withForeignPtr relativeUrl $ \ relGurl -> do
         result <- I.resolve relGurl fullUrl
         foreignPtr <- newForeignPtr I.p'freeUrl result
         return $ FQU foreignPtr
