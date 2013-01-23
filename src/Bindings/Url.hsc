#include <bindings.dsl.h>

#include "url_parse.h"

module Bindings.Url (
  Parsed
) where 

import Foreign.C.Types

#starttype Component
#field begin, CInt
#field end, CInt
#stoptype

#starttype Parsed
#field scheme, Component
#field username, Component
#field password, Component
#field host, Component
#field port, Component
#field path, Component
#field query, Component
#field ref, Component
#field inner_parsed_, (Ptr Parsed)
#stoptype
