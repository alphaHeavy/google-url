#include <bindings.dsl.h>
#include "wrapper.h"

module Bindings.Url (
  c'parseUrl,
  ParsedPrime,
  ParsedPtr
) where 

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data ParsedPrime

type ParsedPtr = Ptr ParsedPrime

{-instance Storable ParsedPtr where
  sizeOf _    = 
  alignment _ = 4-}

#ccall parseUrl , CString -> CSize -> Ptr ParsedPtr -> IO ()
