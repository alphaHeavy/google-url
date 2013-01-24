#ifndef _WRAPPER_H_
#define _WRAPPER_H_

#ifdef __cplusplus
extern "C" {
#endif
  struct Result;

  enum UrlType {
    STANDARD,
    HOST_RELATIVE
  };

  typedef struct Result {
    enum UrlType urlType;
    struct Parsed* urlParsed;
  } Result;

  Result parseUrl(const char *url,const int url_len);
#ifdef __cplusplus
    //throw ();
#else
    ;
#endif

#ifdef __cplusplus
}
#endif

#endif // _WRAPPER_H_
