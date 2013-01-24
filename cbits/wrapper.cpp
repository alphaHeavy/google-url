#include "wrapper.h"
#include "url_parse.h"

using namespace url_parse;

void parseUrl(const char *url, const int url_len, Result *res)
{
  Component scheme;
  if(!ExtractScheme(url, url_len, &scheme))
    return;
  char* schemeString = (char*)malloc(scheme.len);
  strncpy(const_cast<char*>(url)+scheme.begin,schemeString, scheme.len);
  Parsed *result = new Parsed();

  if(schemeString == "http")
  {  
    free(schemeString);
    res->urlType = STANDARD;
    ParseStandardURL(url,url_len,result);
    res->urlParsed = result;
  }
  else
  {
    //result = NULL;
  }
}
