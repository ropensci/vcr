# has nice print method

    Code
      Cassette$new("test")
    Output
      <vcr - Cassette> test
        Record method: once
        Serialize with: yaml
        Re-record interval (s): 
        Clean outdated interactions?: FALSE
        allow_playback_repeats: FALSE
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
      [Cassette: "test"] Inserting: loading 0 interactions from disk
      [Cassette: "test"]   record: once
      [Cassette: "test"]   serialize_with: yaml
      [Cassette: "test"]   allow_playback_repeats: FALSE
      [Cassette: "test"]   preserve_exact_body_bytes: FALSE
      [Cassette: "test"] Handling request: GET {httpbin}/html
      [Cassette: "test"]   looking for existing requests using method/uri
      [Cassette: "test"]   no matching requests
      [Cassette: "test"]   recording response: 200 with 1443 bytes of text/html data
      [Cassette: "test"] Ejecting
    Code
      use_cassette("test", httr::GET(hb("/html")))
    Output
      [Cassette: "test"] Inserting: loading 1 interactions from disk
      [Cassette: "test"]   record: once
      [Cassette: "test"]   serialize_with: yaml
      [Cassette: "test"]   allow_playback_repeats: FALSE
      [Cassette: "test"]   preserve_exact_body_bytes: FALSE
      [Cassette: "test"] Handling request: GET {httpbin}/html
      [Cassette: "test"]   looking for existing requests using method/uri
      [Cassette: "test"]   match: GET {httpbin}/html
      [Cassette: "test"]   matched response 1
      [Cassette: "test"] Ejecting
    Code
      try(use_cassette("test", httr::GET(hb("/404"))), silent = TRUE)
    Output
      [Cassette: "test"] Inserting: loading 1 interactions from disk
      [Cassette: "test"]   record: once
      [Cassette: "test"]   serialize_with: yaml
      [Cassette: "test"]   allow_playback_repeats: FALSE
      [Cassette: "test"]   preserve_exact_body_bytes: FALSE
      [Cassette: "test"] Handling request: GET {httpbin}/404
      [Cassette: "test"]   looking for existing requests using method/uri
      [Cassette: "test"]   no match: GET {httpbin}/404
      [Cassette: "test"]   `matching$uri$path`: "/404" 
      [Cassette: "test"]   `recorded$uri$path`: "/html"
      [Cassette: "test"]   no matching requests
      [Cassette: "test"] Ejecting

