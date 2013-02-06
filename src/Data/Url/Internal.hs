{-# LANGUAGE OverloadedStrings #-}

module Data.Url.Internal (
  Data.Url.Internal.getScheme,
  Data.Url.Internal.getHostname,
  Data.Url.Internal.setScheme,
  p'freeUrl,
  isStandard,
  isValid,
  parseUrl,
  resolve,
  toText) where

import Bindings.Url
import Control.Exception (mask_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Url.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

--toBool :: CInt -> Bool
toBool int =
  case int of
    0 -> False
    1 -> True
    _ -> error "toBool: invalid value"

makeScheme :: Text -> Scheme
makeScheme txt =
  case T.toLower txt of
    "http" -> Http
    "https" -> Https
    _ -> error "Invalid Scheme"


getScheme :: GurlPtr -> IO Scheme
getScheme gurl = do
  str <- c'getScheme gurl
  str' <- peekCString str
  return $ makeScheme $ T.pack str'

setScheme :: GurlPtr -> Scheme -> IO GurlPtr
setScheme gurl scheme = do
  cstr <- newCString $ show scheme 
  c'setScheme gurl cstr

getHostname :: GurlPtr -> IO Hostname
getHostname gurl = do
  str <- c'getHostname gurl
  str' <- peekCString str
  return $ Hostname $ T.pack str'

isStandard :: GurlPtr -> IO Bool
isStandard gurl = do
  result <- c'isStandard gurl
  return $ toBool result

isValid :: GurlPtr -> IO Bool
isValid gurl = do
  result <- c'isValid gurl
  return $ toBool result

parseUrl :: Text -> IO GurlPtr
parseUrl url = do
  cstr <- newCString $ T.unpack url
  alloca $ \ ptr -> do
    c'parseUrl cstr (CSize $ fromIntegral $ T.length url) ptr
    val <- peek ptr
    return val

resolve :: GurlPtr -> GurlPtr -> IO GurlPtr
resolve relativeUrl gurl = do
  result <- c'resolve relativeUrl gurl
  return result

toText :: GurlPtr -> IO Text
toText gurl = do
  str <- c'toString gurl
  str' <- peekCString str
  return $ T.pack str'
