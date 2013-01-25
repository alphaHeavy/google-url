{-# LANGUAGE GADTs #-}

module Data.Url (
  parseUrl,
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
  FullyQualifiedUrl :: ForeignPtr ParsedPrime -> Url

parseUrl :: Text -> Maybe Url
parseUrl str = 
  unsafePerformIO $ 
    mask_ $ do
      cstr <- newCString $ T.unpack str
      alloca $ \ ptr -> do
        c'parseUrl cstr (CSize $ fromIntegral $ T.length str) ptr
        val <- peek ptr
        foreignPtr <- newForeignPtr finalizerFree val
        return $ Just $ FullyQualifiedUrl foreignPtr
