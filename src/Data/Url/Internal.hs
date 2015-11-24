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
  Data.Url.Internal.getPassword,
  Data.Url.Internal.hasPassword,
  Data.Url.Internal.getUsername,
  Data.Url.Internal.setPort,
  Data.Url.Internal.toText,
  Data.Url.Internal.getPath,
  Data.Url.Internal.getPathForRequest,
  Data.Url.Internal.hasPort,
  Data.Url.Internal.hasUsername,
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
import Data.ByteString.Unsafe (unsafePackMallocCString)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Foreign as T
import Data.Url.Types
import Foreign.C.String
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
    "mailto" -> Mail
    "file" -> File
    x -> Unknown x

hasScheme :: GurlPtr -> IO Bool
hasScheme gurl = do
  result <- c'hasScheme gurl
  return $ toBool result

getScheme :: GurlPtr -> IO Scheme
getScheme gurl = do
  bs <- unsafePackMallocCString =<< c'getScheme gurl
  return . makeScheme $! T.decodeUtf8 bs

setScheme :: GurlPtr -> Scheme -> IO GurlPtr
setScheme gurl scheme = do
  withCString (show scheme) $ \ cstr ->
    c'setScheme gurl cstr

getUsername :: GurlPtr -> IO (Maybe ByteString)
getUsername gurl = do
  has <- c'hasUsername gurl
  case has of
    1 -> do
      x <- unsafePackMallocCString =<< c'getUsername gurl
      return $ Just x
    _ -> return Nothing

hasUsername :: GurlPtr -> IO Bool
hasUsername gurl = return . toBool =<< c'hasUsername gurl

getPassword :: GurlPtr -> IO (Maybe ByteString)
getPassword gurl = do
  has <- c'hasPassword gurl
  case has of
    1 -> do
      x <- unsafePackMallocCString =<< c'getPassword gurl
      return $ Just x
    _ -> return Nothing

hasPassword :: GurlPtr -> IO Bool
hasPassword gurl = return . toBool =<< c'hasPassword gurl

getHostname :: GurlPtr -> IO Hostname
getHostname gurl = do
  bs <- unsafePackMallocCString =<< c'getHostname gurl
  return . Hostname $! T.decodeUtf8 bs

hasPort :: GurlPtr -> IO Bool
hasPort gurl = do
  result <- c'hasPort gurl
  return $ toBool result

getPort :: GurlPtr -> IO Port
getPort gurl = do
  i <- c'getPort gurl
  return . Port $! fromIntegral i

getEffectivePort :: GurlPtr -> IO Port
getEffectivePort gurl = do
  i <- c'getEffectivePort gurl
  return . Port $! fromIntegral i

setPort :: GurlPtr -> Port -> IO GurlPtr
setPort gurl (Port port) = do
  withCString (show port) $ \ cstr ->
    c'setPort gurl cstr

getPath :: GurlPtr -> IO Path
getPath gurl = do
  bs <- getPathRaw gurl
  return . Path $! L.drop 1 $ T.splitOn "/" $ T.decodeUtf8 bs

getPathRaw :: GurlPtr -> IO ByteString
getPathRaw gurl =
  unsafePackMallocCString =<< c'getPath gurl

getPathForRequest :: GurlPtr -> IO ByteString
getPathForRequest gurl =
  unsafePackMallocCString =<< c'getPathForRequest gurl

getQuery :: GurlPtr -> IO Query
getQuery gurl = do
  bs <- getQueryRaw gurl
  return . Query $! fmap (\ [k,v] -> (k,if T.null v then Nothing else Just v)) $ L.filter (\ x -> L.length x > 1) $ fmap (T.splitOn "=") $ T.splitOn "&" $ T.decodeUtf8 bs

getQueryRaw :: GurlPtr -> IO ByteString
getQueryRaw gurl =
  unsafePackMallocCString =<< c'getQuery gurl

getFragment :: GurlPtr -> IO Fragment
getFragment gurl = do
  bs <- getFragmentRaw gurl
  return . Fragment $! T.decodeUtf8 bs

getFragmentRaw :: GurlPtr -> IO ByteString
getFragmentRaw gurl =
  unsafePackMallocCString =<< c'getFragment gurl

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
  bs <- unsafePackMallocCString =<< c'toString gurl
  return $! T.decodeUtf8 bs

equals :: GurlPtr -> GurlPtr -> IO Bool
equals ptr1 ptr2 = do
  result <- c'equals ptr1 ptr2
  return $ toBool result
