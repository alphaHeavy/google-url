#include "wrapper.h"
#include "gurl.h"
#include <string>

using namespace std;

void parseUrl(const char *url, const int url_len, void **res)
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

char* toString(const void *gurl)
{
  GURL *url = (GURL*)gurl;
  return strdup (url->spec().c_str());
}
