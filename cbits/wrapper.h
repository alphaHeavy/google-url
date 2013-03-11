#ifndef _WRAPPER_H_
#define _WRAPPER_H_

#ifdef __cplusplus
extern "C" {
#endif
  void parseUrl(const char *url,const int url_len, void **result);

  int isStandard(const void *gurl);

  char* getScheme(const void *gurl);
  char* getHostname(const void *gurl);
  char* getUsername(const void *gurl);
  char* getPassword(const void *gurl);
  int   getPort(const void *gurl);
  int   getEffectivePort(const void *gurl);
  char* getPath(const void *gurl);
  char* getQuery(const void *gurl);
  char* getFragment(const void *gurl);

  void* setScheme(const void *gurl, const char *scheme);
  void* setPort(const void *gurl, const char *port);

  int hasScheme(const void *gurl);
  int hasUsername(const void *gurl);
  int hasPassword(const void *gurl);
  int hasHostname(const void *gurl);
  int hasPort(const void *gurl);
  int hasPath(const void *gurl);
  int hasQuery(const void *gurl);
  int hasFragment(const void *gurl);

  char* getFilename(const void *gurl);
  char* getPathForRequest(const void *gurl);
  char* getHostNoBrackets(const void *gurl);
  int domainIs (const char *domain, const void *gurl);

  void swap(void *gurl, const void *other);

  void* getWithEmptyPath(const void *gurl);

  void* getOrigin(const void *gurl);  

  int schemeIs(const char *scheme, const void *gurl);
  int schemeIsFile(const void *gurl);
  int schemeIsFileSystem(const void *gurl);
  int schemeIsSecure(const void *gurl);

  int hostIsIPAddress(const void *gurl);

  void* resolve(const void* relative, const void *gurl);
  int isValid(const void *gurl);
  int equals(const void *gurl1, const void *gurl2);

  char* toString(const void *gurl);

  void freeUrl(void *gurl);
#ifdef __cplusplus
    //throw ();
#else
    ;
#endif

#ifdef __cplusplus
}
#endif

#endif // _WRAPPER_H_
