# inserting and ejecting is logged

    Code
      . <- use_cassette("test", NULL)
    Output
      [Cassette: "test"] - Inserting
      [Cassette: "test"] - Init. HTTPInteractionList w/ request matchers [method, uri] & 0 interaction(s): {  }
      [Cassette: "test"] - Initialized with options: {name: test, record: once, serialize_with: yaml, match_requests_on: c("method", "uri"), allow_playback_repeats: FALSE, preserve_exact_body_bytes: FALSE}
      [Cassette: "test"] - Ejecting

