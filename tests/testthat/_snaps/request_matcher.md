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

# default matcher includes body_json

    Code
      use_cassette("test", httr2::req_perform(req2))
    Output
      [Cassette: test] Inserting 'test.yml' (with 1 interactions)
      [Cassette: test]   Mode: replaying
      [Cassette: test] Handling request: POST {httpbin}/post
      [Cassette: test]   Looking for existing requests using method/uri/body_json
      [Cassette: test]     Request 1: NO MATCH
      [Cassette: test]       `matching$body$foo`: "baz"
      [Cassette: test]       `recorded$body$foo`: "bar"
      [Cassette: test]   No matching requests
    Condition
      Error:
      ! Failed to find matching request in active cassette, "test".
      i Learn more in `vignette(vcr::debugging)`.

# default matcher includes body

    Code
      use_cassette("test", httr2::req_perform(req2))
    Output
      [Cassette: test] Inserting 'test.yml' (with 1 interactions)
      [Cassette: test]   Mode: replaying
      [Cassette: test] Handling request: POST {httpbin}/post foo=baz
      [Cassette: test]   Looking for existing requests using method/uri/body
      [Cassette: test]     Request 1: NO MATCH
      [Cassette: test]       `matching$body`: "foo=baz"
      [Cassette: test]       `recorded$body`: "foo=bar"
      [Cassette: test]   No matching requests
    Condition
      Error:
      ! Failed to find matching request in active cassette, "test".
      i Learn more in `vignette(vcr::debugging)`.

