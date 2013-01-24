#include "wrapper.h"
#include "url_parse.h"

using namespace url_parse;

void parseUrl(const char *url, const int url_len, Result *res)
{
  Component scheme;
  if(!ExtractScheme(url, url_len, &scheme))
    return;
  url_parse::Parsed *result = new url_parse::Parsed();

  if (strncmp(&url[scheme.begin], "http", scheme.len) == 0)
  {
    res->urlType = STANDARD;
    ParseStandardURL(url,url_len,result);
    res->urlParsed = reinterpret_cast< ::Parsed*>(result);
  }
  else
  {
    //result = NULL;
  }
}
