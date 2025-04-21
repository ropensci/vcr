# inserting and ejecting is logged

    Code
      . <- use_cassette("test", NULL)
    Output
      [Cassette: "test"] - Inserting
      [Cassette: "test"] - Init. HTTPInteractionList w/ request matchers [method, uri] & 0 interaction(s): {  }
      [Cassette: "test"] - Initialized with options: {name: test, record: once, serialize_with: yaml, match_requests_on: c("method", "uri"), allow_playback_repeats: FALSE, preserve_exact_body_bytes: FALSE}
      [Cassette: "test"] - Ejecting
    Condition
      Warning:
      x "test" cassette ejected without recording any interactions.
      i Did your request error?
      i Did you use {curl}, `download.file()`, or other unsupported tool?
      i If you are using crul/httr/httr2, are you sure you made an HTTP request?

