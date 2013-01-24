#include <bindings.dsl.h>
#include "wrapper.h"

module Bindings.Url (
  c'parseUrl,
  ParsedPrime,
  ParsedPtr,
  C'Result(..),
  UrlType(..)
) where 

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data ParsedPrime

type ParsedPtr = Ptr ParsedPrime

newtype UrlType = UrlType {t :: CInt} deriving (Eq,Show)

instance Storable UrlType where
  sizeOf _    = 4
  alignment _ = 4

#{enum UrlType, UrlType,
  standard = STANDARD
}

#starttype Result
#field urlType , UrlType
#field urlParsed , ParsedPtr
#stoptype  

#ccall parseUrl , CString -> CSize -> Ptr C'Result -> IO ()
