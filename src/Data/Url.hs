{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Url (
  isStandard,
  parseUrl,
  parseFullyQualifiedUrl,
  resolveRelativeUrl,
  toText,
  HasHostname(..),
  HasPath(..),
  HasPort(..),
  HasQuery(..),
  HasFragment(..),
  HasScheme(..),
  Url(..),
  module Data.Url.Types) where

import Control.Exception (mask_)
import Control.Monad.Trans
import Data.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Url.Internal as I
import Data.Url.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe


instance HasScheme Url where
  hasScheme (FullyQualifiedUrl _) = True
  getScheme (FullyQualifiedUrl gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getScheme val
  setScheme (FullyQualifiedUrl gurl) scheme = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    result <- I.setScheme val scheme
    foreignPtr <- newForeignPtr I.p'freeUrl result
    return $ FullyQualifiedUrl foreignPtr


instance HasHostname Url where
  getHostname (FullyQualifiedUrl gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getHostname val
  hasHostname (FullyQualifiedUrl _) = True

instance Eq Url where
  (FullyQualifiedUrl url1) == (FullyQualifiedUrl url2) = unsafePerformIO $ mask_ $ withForeignPtr url1 $ \ val1 -> withForeignPtr url2 $ \ val2 -> I.equals val1 val2
  (RelativeUrl url1) == (RelativeUrl url2) = unsafePerformIO $ mask_ $ withForeignPtr url1 $ \ val1 -> withForeignPtr url2 $ \ val2 -> I.equals val1 val2
  (InvalidUrl url1) == (InvalidUrl url2) = unsafePerformIO $ mask_ $ withForeignPtr url1 $ \ val1 -> withForeignPtr url2 $ \ val2 -> I.equals val1 val2
  (FileUrl url1) == (FileUrl url2) = unsafePerformIO $ mask_ $ withForeignPtr url1 $ \ val1 -> withForeignPtr url2 $ \ val2 -> I.equals val1 val2
  _ == _ = False

instance Show Url where
  show = T.unpack . toText

parseUrl :: Text -> Url
parseUrl str = 
  unsafePerformIO $ 
    mask_ $ do
      parsed <- I.parseUrl str
      foreignPtr <- newForeignPtr I.p'freeUrl parsed
      valid <- I.isValid parsed
      case valid of
        False -> return $ InvalidUrl foreignPtr
        True -> do
          standard <- I.isStandard  parsed
          case standard of
            False -> return $ RelativeUrl foreignPtr
            True -> return $ FullyQualifiedUrl foreignPtr

{- This function is unsafe. It assumes that the string is fully qualified url. Use with caution. -}
parseFullyQualifiedUrl :: Text -> Url
parseFullyQualifiedUrl str =
  unsafePerformIO $ 
    mask_ $ do
      parsed <- I.parseUrl str
      foreignPtr <- newForeignPtr I.p'freeUrl parsed
      return $ FullyQualifiedUrl foreignPtr

resolveRelativeUrl :: Url -> Url -> Url
resolveRelativeUrl (FullyQualifiedUrl gurl) (RelativeUrl relativeUrl) =
  unsafePerformIO $ mask_ $ 
    withForeignPtr gurl $ \ fullUrl ->
      withForeignPtr relativeUrl $ \ relGurl -> do
         result <- I.resolve relGurl fullUrl
         foreignPtr <- newForeignPtr I.p'freeUrl result
         return $ FullyQualifiedUrl foreignPtr

toText :: Url -> Text
toText url = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val
  where
    gurl = case url of
             (FullyQualifiedUrl ptr) -> ptr
             (RelativeUrl ptr) -> ptr

isStandard :: Url -> Bool
isStandard url = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val
  where
    gurl = case url of
             (FullyQualifiedUrl ptr) -> ptr
             (RelativeUrl ptr) -> ptr

