#include <bindings.dsl.h>
#include "wrapper.h"

module Bindings.Url (
  c'freeUrl,
  c'getEffectivePort,
  c'getFilename,
  c'getFragment,
  c'getHostname,
  c'getOrigin,
  c'getPassword,
  c'getPath,
  c'getPathForRequest,
  c'getPort,
  c'getQuery,
  c'getScheme,
  c'getUsername,
  c'hasFragment,
  c'hasHostname,
  c'hasPassword,
  c'hasPath,
  c'hasPort,
  c'hasQuery,
  c'hasScheme,
  c'hasUsername,
  c'isStandard,
  c'isValid,
  c'parseUrl,
  c'resolve,
  c'swap,
  c'toString,
  Gurl,
  GurlPtr,
  p'freeUrl
) where 

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data Gurl

type GurlPtr = Ptr Gurl

#ccall parseUrl , CString -> CSize -> Ptr GurlPtr -> IO ()
#ccall isStandard , GurlPtr -> IO Int
#ccall toString , GurlPtr -> IO CString
#ccall freeUrl , GurlPtr -> IO ()
#ccall getScheme , GurlPtr -> IO CString
#ccall getHostname , GurlPtr -> IO CString
#ccall getUsername , GurlPtr -> IO CString
#ccall getPassword , GurlPtr -> IO CString
#ccall getPort , GurlPtr -> IO CInt
#ccall getEffectivePort , GurlPtr -> IO CInt
#ccall getPath , GurlPtr -> IO CString
#ccall getQuery , GurlPtr -> IO CString
#ccall getFragment , GurlPtr -> IO CString
#ccall hasScheme , GurlPtr -> IO CInt
#ccall hasUsername , GurlPtr -> IO CInt
#ccall hasPassword , GurlPtr -> IO CInt
#ccall hasHostname , GurlPtr -> IO CInt
#ccall hasPort , GurlPtr -> IO CInt
#ccall hasPath , GurlPtr -> IO CInt
#ccall hasQuery , GurlPtr -> IO CInt
#ccall hasFragment , GurlPtr -> IO CInt

#ccall getFilename , GurlPtr -> IO CString
#ccall getPathForRequest , GurlPtr -> IO CString
#ccall getHostNoBrackets , GurlPtr -> IO CString
#ccall domainIs , CString -> GurlPtr -> IO CInt
#ccall swap , GurlPtr -> GurlPtr -> IO ()
#ccall getWithEmptyPath , GurlPtr -> IO GurlPtr
#ccall getOrigin , GurlPtr -> IO GurlPtr
#ccall schemeIs , CString -> GurlPtr -> IO CInt
#ccall schemeIsFile , GurlPtr -> IO CInt
#ccall schemeIsFileSystem , GurlPtr -> IO CInt
#ccall schemeIsSecure , GurlPtr -> IO CInt
#ccall hostIsIPAddress , GurlPtr -> IO CInt
#ccall resolve , GurlPtr -> GurlPtr -> IO GurlPtr
#ccall isValid , GurlPtr -> IO CInt
