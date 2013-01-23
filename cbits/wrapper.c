include "wrapper.h"

void parseUrl(const char *url, const int url_len, Parsed* result)
{
  Component scheme;
  if(!ExtractScheme(url, url_len, &scheme))
    return -1;
  if(scheme == "http")
  {

    
  }
}
