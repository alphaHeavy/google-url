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
  url_parse::Parsed *result = new url_parse::Parsed();

  if (strcmp(schemeString, "http"))
  {
    free(schemeString);
    res->urlType = STANDARD;
    ParseStandardURL(url,url_len,result);
    res->urlParsed = reinterpret_cast< ::Parsed*>(result);
  }
  else
  {
    //result = NULL;
  }
}
