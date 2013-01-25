#ifndef _WRAPPER_H_
#define _WRAPPER_H_

#ifdef __cplusplus
extern "C" {
#endif
  void parseUrl(const char *url,const int url_len, void **result);

  int isStandard(const void *gurl);

  void getScheme(const void *gurl, char**);

  char* toString(const void *gurl);
#ifdef __cplusplus
    //throw ();
#else
    ;
#endif

#ifdef __cplusplus
}
#endif

#endif // _WRAPPER_H_
