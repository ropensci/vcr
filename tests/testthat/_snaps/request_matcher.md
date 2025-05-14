# request_matches has useful logging

    Code
      . <- request_matches(req1, req1)
    Output
      [Cassette: <none>]     Request 1: MATCH
    Code
      . <- request_matches(req1, req2)
    Output
      [Cassette: <none>]     Request 1: NO MATCH
      [Cassette: <none>]       `matching$uri$path`: ""    
      [Cassette: <none>]       `recorded$uri$path`: "/foo"
    Code
      . <- request_matches(req1, req3)
    Output
      [Cassette: <none>]     Request 1: NO MATCH
      [Cassette: <none>]       `matching$method`: "GET" 
      [Cassette: <none>]       `recorded$method`: "POST"
    Code
      . <- request_matches(req1, req4)
    Output
      [Cassette: <none>]     Request 1: NO MATCH
      [Cassette: <none>]       `matching$method`: "GET" 
      [Cassette: <none>]       `recorded$method`: "POST"
      [Cassette: <none>]       `matching$uri$path`: ""    
      [Cassette: <none>]       `recorded$uri$path`: "/foo"

# informative feedback for components that are absent

    Code
      . <- request_matches(req1, req2)
    Output
      [Cassette: <none>]     Request 1: NO MATCH
      [Cassette: <none>]       `matching$uri$params` is length 0
      [Cassette: <none>]       `recorded$uri$params` is length 1
      [Cassette: <none>]       `names(matching$uri$params)`:    
      [Cassette: <none>]       `names(recorded$uri$params)`: "q"
      [Cassette: <none>]       `matching$uri$params$q` is absent
      [Cassette: <none>]       `recorded$uri$params$q` is a character vector ('1')

