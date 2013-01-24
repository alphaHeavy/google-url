#include <bindings.dsl.h>
#include "wrapper.h"


module Bindings.Url (
  c'parseUrl,
  ParsedPtr,
  Result(..)
) where 

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr


data Parsed

type ParsedPtr = Ptr Parsed

newtype UrlType = UrlType {t :: CInt}

#{enum UrlType, UrlType,
  standard = STANDARD
}

#starttype Result
#field urlType , UrlType
#field urlParsed , ParsedPtr
#stoptype

#ccall parseUrl , CString -> CSize -> IO Result
