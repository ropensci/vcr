# request_matches has useful logging

    Code
      . <- request_matches(req1, req1)
    Output
      [Cassette: <none>]   method/uri match: GET http://example.com
    Code
      . <- request_matches(req1, req2)
    Output
      [Cassette: <none>]   no method/uri match {GET http://example.com}:
      `playing$uri$path`:  ""    
      `recorded$uri$path`: "/foo"
    Code
      . <- request_matches(req1, req3)
    Output
      [Cassette: <none>]   no method/uri match {GET http://example.com}:
      `playing$method`:  "GET" 
      `recorded$method`: "POST"
    Code
      . <- request_matches(req1, req4)
    Output
      [Cassette: <none>]   no method/uri match {GET http://example.com}:
      `playing$method`:  "GET" 
      `recorded$method`: "POST"
      `playing$uri$path`:  ""    
      `recorded$uri$path`: "/foo"

