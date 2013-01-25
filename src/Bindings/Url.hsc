#include <bindings.dsl.h>
#include "wrapper.h"

module Bindings.Url (
  c'isStandard,
  c'parseUrl,
  c'toString,
  Gurl,
  GurlPtr
) where 

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data Gurl

type GurlPtr = Ptr Gurl

#ccall parseUrl , CString -> CSize -> Ptr GurlPtr -> IO ()
#ccall isStandard , GurlPtr -> IO Int
#ccall toString , GurlPtr -> IO CString
