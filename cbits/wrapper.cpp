#include "wrapper.h"
#include "gurl.h"
#include "url_canon.h"
#include <string>

using namespace std;
using namespace url_canon;

void parseUrl(const char *url, const size_t url_len, void **res)
{
  string str = string(url,url_len);
  GURL *gurl = new GURL(str);
  *res = (void *)gurl;
}

int isStandard(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return (int)url->IsStandard();
}

void freeUrl (void *gurl)
{
  GURL *url = (GURL*)gurl;
  delete url;
}

char* toString(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup (url->spec().c_str());
}

char* getScheme(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->scheme().c_str());
}

char* getHostname(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->host().c_str());
}

char* getUsername(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->username().c_str());
}

char* getPassword(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->password().c_str());
}

int   getPort(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->IntPort();
}

int   getEffectivePort(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->EffectiveIntPort();
}

char* getPath(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->path().c_str());
}

char* getQuery(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->query().c_str());
}

char* getFragment(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->ref().c_str());
}

int hasScheme(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->has_scheme();
}

int hasUsername(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->has_username();
}

int hasPassword(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->has_password();
}

int hasHostname(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->has_host();
}

int hasPort(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->has_port();
}

int hasPath(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->has_path();
}

int hasQuery(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->has_query();
}

int hasFragment(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->has_ref();
}

char* getFilename(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->ExtractFileName().c_str());
}

char* getPathForRequest(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->PathForRequest().c_str());
}

char* getHostNoBrackets(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup(url->HostNoBrackets().c_str());
}

int domainIs (const char *domain, const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->DomainIs(domain);
}

void swap(void *gurl, const void *other)
{
  GURL *url = (GURL*)gurl;
  GURL *otherUrl = (GURL*)other;
  url->Swap(otherUrl);
}

void* getWithEmptyPath(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  GURL newUrl = url->GetWithEmptyPath();
  return new GURL(newUrl);
}

void* getOrigin(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  GURL newUrl = url->GetOrigin();
  return new GURL(newUrl);
}

int schemeIs(const char *scheme, const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->SchemeIs(scheme);
}

int schemeIsFile(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->SchemeIsFile();
}

int schemeIsFileSystem(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->SchemeIsFileSystem();
}

int schemeIsSecure(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->SchemeIsSecure();
}

int hostIsIPAddress(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return url->HostIsIPAddress();
}

void* resolve(const char* relative, size_t length, const void *gurl)
{
  const string str(relative, length);
  GURL *url = (GURL *)gurl;
  GURL newUrl = url->Resolve(str);
  return new GURL(newUrl);
}

int isValid(const void *gurl)
{
  GURL *url = (GURL *)gurl;
  return url->is_valid();
}

void* setScheme (const void *gurl, const char *scheme)
{
  GURL *ptr = (GURL *)gurl;
  Replacements<char> r;
  r.SetScheme(scheme,ptr->parsed_for_possibly_invalid_spec().scheme);
  GURL result = ptr->ReplaceComponents(r);
  return (void *) new GURL(result);
}

void* setPort (const void *gurl, const char *port)
{
  GURL *ptr = (GURL *)gurl;
  Replacements<char> r;
  r.SetPort(port, ptr->parsed_for_possibly_invalid_spec().port);
  GURL result = ptr->ReplaceComponents(r);
  return (void *) new GURL(result);
}

int equals(const void *gurl1, const void *gurl2)
{
  GURL *ptr1 = (GURL *)gurl1;
  GURL *ptr2 = (GURL *)gurl2;
  return *ptr1 == *ptr2;
}
