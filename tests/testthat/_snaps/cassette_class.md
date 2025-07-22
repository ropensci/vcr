# has nice print method

    Code
      Cassette$new("test")
    Output
      <vcr - Cassette> test
        Record method: once
        Serialize with: yaml
        preserve_exact_body_bytes: FALSE

# cassette warns if ejected with no interactions

    Code
      . <- cl$eject()
    Condition
      Warning:
      x "test" cassette ejected without recording any interactions.
      i Did you use {curl}, `download.file()`, or other unsupported tool?
      i If you are using crul/httr/httr2, are you sure you made an HTTP request?

# important interactions are logged

    Code
      use_cassette("test", httr::GET(hb("/html")))
    Output
      [Cassette: test] Inserting 'test.yml' (new cassette)
      [Cassette: test]   Mode: recording
      [Cassette: test] Handling request: GET {httpbin}/html
      [Cassette: test]   Recording response: 200 with 1443 bytes of text/html data
      [Cassette: test] Ejecting
    Code
      use_cassette("test", httr::GET(hb("/html")))
    Output
      [Cassette: test] Inserting 'test.yml' (with 1 interactions)
      [Cassette: test]   Mode: replaying
      [Cassette: test] Handling request: GET {httpbin}/html
      [Cassette: test]   Looking for existing requests using method/uri
      [Cassette: test]     Request 1: MATCH
      [Cassette: test]   Replaying response 1
      [Cassette: test] Ejecting
    Code
      use_cassette("test", httr::GET(hb("/404")))
    Output
      [Cassette: test] Inserting 'test.yml' (with 1 interactions)
      [Cassette: test]   Mode: replaying
      [Cassette: test] Handling request: GET {httpbin}/404
      [Cassette: test]   Looking for existing requests using method/uri
      [Cassette: test]     Request 1: NO MATCH
      [Cassette: test]       `matching$uri$path`: "/404" 
      [Cassette: test]       `recorded$uri$path`: "/html"
      [Cassette: test]   No matching requests
    Condition
      Error:
      ! Failed to find matching request in active cassette, "test".
      i Learn more in `vignette(vcr::debugging)`.

