#ifndef _WRAPPER_H_
#define _WRAPPER_H_

#include "google-url/src/url_parse.h"

#ifdef __cplusplus
extern "C" {
#endif
 void parseUrl(const char *url,const int url_len, Parsed* result);
#ifdef __cplusplus
    throw ();
#else
    ;
#endif

#ifdef __cplusplus
}
#endif

#endif // _XPATHPARSERS_PARSE_H_
