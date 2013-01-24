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
import Foreign.C.String (withCString)
import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

data Url where
  FullyQualifiedUrl :: ByteString -> ParsedPtr -> Url

parseUrl :: Text -> Maybe Url
parseUrl str = 
  unsafePerformIO $ 
    mask_ $
      withCString (T.unpack str) $ \ cstr -> do
        struct <- malloc
        c'parseUrl cstr (CSize $ fromIntegral $ T.length str) struct
        val <- peek struct
        foreignPtr <- newForeignPtr finalizerFree $ c'Result'urlParsed val
        undefined
