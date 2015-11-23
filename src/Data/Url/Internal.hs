{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Url.Internal (
  Data.Url.Internal.equals,
  Data.Url.Internal.getScheme,
  Data.Url.Internal.getFragment,
  Data.Url.Internal.getHostname,
  Data.Url.Internal.setScheme,
  Data.Url.Internal.getPort,
  Data.Url.Internal.getEffectivePort,
  Data.Url.Internal.setPort,
  Data.Url.Internal.toText,
  Data.Url.Internal.getPath,
  Data.Url.Internal.getPathForRequest,
  Data.Url.Internal.hasPort,
  Data.Url.Internal.getQuery,
  Data.Url.Internal.hasScheme,
  p'freeUrl,
  Data.Url.Internal.isStandard,
  Data.Url.Internal.isValid,
  parseUrl,
  resolve,
  getPathRaw,
  getFragmentRaw,
  getQueryRaw
  ) where

import Bindings.Url
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Url.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

toBool :: forall a. (Eq a, Num a) => a -> Bool
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

hasScheme :: GurlPtr -> IO Bool
hasScheme gurl = do
  result <- c'hasScheme gurl
  return $ toBool result

getScheme :: GurlPtr -> IO Scheme
getScheme gurl = do
  str <- c'getScheme gurl
  str' <- peekCString str
  free str
  return $ makeScheme $ T.pack str'

setScheme :: GurlPtr -> Scheme -> IO GurlPtr
setScheme gurl scheme = do
  withCString (show scheme) $ \ cstr ->
    c'setScheme gurl cstr

getHostname :: GurlPtr -> IO Hostname
getHostname gurl = do
  str <- c'getHostname gurl
  str' <- peekCString str
  free str
  return $ Hostname $ T.pack str'

hasPort :: GurlPtr -> IO Bool
hasPort gurl = do
  result <- c'hasPort gurl
  return $ toBool result

getPort :: GurlPtr -> IO Port
getPort gurl = do
  i <- c'getPort gurl
  return $ Port $ fromIntegral i

getEffectivePort :: GurlPtr -> IO Port
getEffectivePort gurl = do
  i <- c'getEffectivePort gurl
  return $ Port $ fromIntegral i

setPort :: GurlPtr -> Port -> IO GurlPtr
setPort gurl (Port port) = do
  withCString (show port) $ \ cstr ->
    c'setPort gurl cstr

getPath :: GurlPtr -> IO Path
getPath gurl = do
  str <- c'getPath gurl
  str' <- peekCString str
  free str
  return $ Path $ L.drop 1 $ T.splitOn "/" $ T.pack str'

getPathRaw :: GurlPtr -> IO ByteString
getPathRaw gurl = do
  str <- c'getPath gurl
  let str' = B.packCString str
  free str
  str'

getPathForRequest :: GurlPtr -> IO ByteString
getPathForRequest gurl = do
  str <- c'getPathForRequest gurl
  let str' = B.packCString str
  free str
  str'

getQuery :: GurlPtr -> IO Query
getQuery gurl = do
  str <- c'getQuery gurl
  str' <- peekCString str
  free str
  return $ Query $ fmap (\ [k,v] -> (k,if T.null v then Nothing else Just v)) $ L.filter (\ x -> L.length x > 1) $ fmap (T.splitOn "=") $ T.splitOn "&" $ T.pack str'

getQueryRaw :: GurlPtr -> IO ByteString
getQueryRaw gurl = do
  str <- c'getQuery gurl
  let str' = B.packCString str
  free str
  str'

getFragment :: GurlPtr -> IO Fragment
getFragment gurl = do
  str <- c'getFragment gurl
  str' <- peekCString str
  free str
  return $ Fragment $ T.pack str'


getFragmentRaw :: GurlPtr -> IO ByteString
getFragmentRaw gurl = do
  str <- c'getFragment gurl
  let str' = B.packCString str
  free str
  str'

isStandard :: GurlPtr -> IO Bool
isStandard gurl = do
  result <- c'isStandard gurl
  return $ toBool result

isValid :: GurlPtr -> IO Bool
isValid gurl = do
  result <- c'isValid gurl
  return $ toBool result

parseUrl :: Text -> IO GurlPtr
parseUrl url =
  T.withCStringLen url $ \ (cstr, clen) ->
  alloca $ \ ptr -> do
    c'parseUrl cstr (fromIntegral clen) ptr
    val <- peek ptr
    return val

resolve :: Text -> GurlPtr -> IO GurlPtr
resolve relativeUrl gurl = do
  T.withCStringLen relativeUrl $ \ (cstr, clen) ->
    c'resolve cstr (fromIntegral clen) gurl

toText :: GurlPtr -> IO Text
toText gurl = do
  str <- c'toString gurl
  str' <- peekCString str
  return $ T.pack str'

equals :: GurlPtr -> GurlPtr -> IO Bool
equals ptr1 ptr2 = do
  result <- c'equals ptr1 ptr2
  return $ toBool result
