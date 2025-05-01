# checks constructor args

    Code
      Cassette$new()
    Condition
      Error in `initialize()`:
      ! argument "name" is missing, with no default
    Code
      Cassette$new("test", record = "stuff")
    Condition
      Error:
      ! 'record' value of 'stuff' is not in the allowed set: none, once, new_episodes, all
    Code
      Cassette$new("test", match_requests_on = "x")
    Condition
      Error:
      ! 1 or more 'match_requests_on' values (x) is not in the allowed set: method, uri, headers, host, path, body, query
    Code
      Cassette$new("test", serialize_with = "howdy")
    Condition
      Error in `serializer_fetch()`:
      ! Unsupported cassette serializer "howdy".
    Code
      Cassette$new("test", preserve_exact_body_bytes = 5)
    Condition
      Error:
      ! preserve_exact_body_bytes must be of class logical

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

# cassette checks name

    Code
      Cassette$new("foo bar")
    Condition
      Error in `initialize()`:
      ! `name` must not contain spaces.
    Code
      Cassette$new("foo.yml")
    Condition
      Error in `initialize()`:
      ! `name` must not include an extension.
    Code
      Cassette$new("foo/bar")
    Condition
      Error in `initialize()`:
      ! `name` must not contain '/', '?', '<', '>', '\', ':', '*', '|', or '"'
    Code
      Cassette$new("foo\nbar")
    Condition
      Error in `initialize()`:
      ! `name` must not contain spaces.
    Code
      Cassette$new("foo\nbar.")
    Condition
      Error in `initialize()`:
      ! `name` must not contain spaces.
    Code
      Cassette$new("..")
    Condition
      Error in `initialize()`:
      ! `name` must not be '.', '..', etc.
    Code
      Cassette$new("con")
    Condition
      Error in `initialize()`:
      ! `name` must not contain reserved windows strings.
    Code
      Cassette$new(strrep("x", 400))
    Condition
      Error in `initialize()`:
      ! `name` must be less than 256 characters.

---

    Code
      Cassette$new("foo")
    Condition
      Error in `initialize()`:
      ! `name` must not be the same as an existing cassette.

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
      [Cassette: "test"] Ejecting: writing 1 interactions
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

