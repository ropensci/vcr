# request_matches has useful logging

    Code
      . <- request_matches(req1, req1)
    Output
      [Cassette: <none>]   match: GET http://example.com
    Code
      . <- request_matches(req1, req2)
    Output
      [Cassette: <none>]   no match: GET http://example.com
      [Cassette: <none>]   `matching$uri$path`: ""    
      [Cassette: <none>]   `recorded$uri$path`: "/foo"
    Code
      . <- request_matches(req1, req3)
    Output
      [Cassette: <none>]   no match: GET http://example.com
      [Cassette: <none>]   `matching$method`: "GET" 
      [Cassette: <none>]   `recorded$method`: "POST"
    Code
      . <- request_matches(req1, req4)
    Output
      [Cassette: <none>]   no match: GET http://example.com
      [Cassette: <none>]   `matching$method`: "GET" 
      [Cassette: <none>]   `recorded$method`: "POST"
      [Cassette: <none>]   `matching$uri$path`: ""    
      [Cassette: <none>]   `recorded$uri$path`: "/foo"

