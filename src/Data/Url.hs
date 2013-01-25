{-# LANGUAGE GADTs #-}

module Data.Url (
  parseUrl,
  toString,
  Url(..)) where

import Bindings.Url
import Control.Exception (mask_)
import Control.Monad.Trans
import Data.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

data Url where
  FullyQualifiedUrl :: ForeignPtr GurlPtr -> Url
  RelativeUrl :: ForeignPtr GurlPtr -> Url

parseUrl :: Text -> Maybe Url
parseUrl str = 
  unsafePerformIO $ 
    mask_ $ do
      cstr <- newCString $ T.unpack str
      alloca $ \ ptr -> do
        c'parseUrl cstr (CSize $ fromIntegral $ T.length str) ptr
        val <- peek ptr
        foreignPtr <- newForeignPtr finalizerFree ptr
        result <- isStandard' val
        case result of
          0 -> return $ Just $ RelativeUrl foreignPtr
          1 -> return $ Just $ FullyQualifiedUrl foreignPtr
  where
    isStandard' :: GurlPtr -> IO Int
    isStandard' ptr = c'isStandard ptr

toString :: Url -> Text
toString url =
  unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    val' <- peek val
    str <- c'toString val'
    str' <- peekCString str
    free str
    return $ T.pack str'
  where
    gurl = case url of
             (FullyQualifiedUrl ptr) -> ptr
             (RelativeUrl ptr) -> ptr
isStandard :: Url -> Bool
isStandard url =
  unsafePerformIO $ mask_ $ withForeignPtr gurl $ \ val -> do
    val' <- peek val
    result <- c'isStandard val'
    case result of
      0 -> return False
      1 -> return True
  where
    gurl = case url of
             (FullyQualifiedUrl ptr) -> ptr
             (RelativeUrl ptr) -> ptr
