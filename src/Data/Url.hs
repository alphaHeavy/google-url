{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Url (
  isStandard,
  parseUrl,
  resolveRelativeUrl,
  toText,
  HasHostname(..),
  HasPath(..),
  HasPort(..),
  HasQuery(..),
  HasFragment(..),
  HasScheme(..),
  Url(..)) where

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


instance HasScheme (Url 'Full) where
  hasScheme (FullyQualifiedUrl _) = True
  getScheme (FullyQualifiedUrl gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getScheme val

instance HasHostname (Url 'Full) where
  getHostname (FullyQualifiedUrl gurl) = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.getHostname val
  hasHostname (FullyQualifiedUrl _) = True

--data UrlFoo = FullyQualified (Url 'Good) | Canonical (CanonicalUrl 'Good)

parseUrl :: Text -> ParsedUrl --(Url a)
parseUrl str = 
  unsafePerformIO $ 
    mask_ $ do
      parsed <- I.parseUrl str
      foreignPtr <- newForeignPtr I.p'freeUrl parsed
      valid <- I.isValid parsed
      case valid of
        False -> return $ ParsedInvalidUrl $ InvalidUrl foreignPtr
        True -> do
          standard <- I.isStandard  parsed
          case standard of
            False -> return $ ParsedRelativeUrl $ RelativeUrl foreignPtr
            True -> return $ ParsedFullyQualifiedUrl $ FullyQualifiedUrl foreignPtr

resolveRelativeUrl :: Url 'Full -> Url 'Relative -> (Url 'Full)
resolveRelativeUrl (FullyQualifiedUrl gurl) (RelativeUrl relativeUrl) =
  unsafePerformIO $ mask_ $ 
    withForeignPtr gurl $ \ fullUrl ->
      withForeignPtr relativeUrl $ \ relGurl -> do
         result <- I.resolve relGurl fullUrl
         foreignPtr <- newForeignPtr I.p'freeUrl result
         return $ FullyQualifiedUrl foreignPtr

toText :: ValidUrl a ~ 'True => Url a -> Text
toText url = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.toText val
  where
    gurl = case url of
             (FullyQualifiedUrl ptr) -> ptr
             (RelativeUrl ptr) -> ptr

isStandard :: Url a -> Bool
isStandard url = unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> I.isStandard val
  where
    gurl = case url of
             (FullyQualifiedUrl ptr) -> ptr
             (RelativeUrl ptr) -> ptr

