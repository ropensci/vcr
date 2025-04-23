# ejecting errors if no cassettes

    Code
      eject_cassette()
    Condition
      Error in `eject_cassette()`:
      ! No cassette in use.

# inserting a cassette errors when vcr turned off and ignore_cassettes=FALSE

    Code
      insert_cassette("test")
    Condition
      Error in `insert_cassette()`:
      ! vcr is turned off.
      i Use `turn_on()` to turn it back on.
      i Or use `turn_off(ignore_cassettes = TRUE)` to cassettes completely.

# inserting and ejecting is logged

    Code
      . <- use_cassette("test", NULL)
    Output
      [Cassette: "test"] - Inserting
      [Cassette: "test"] - Init. HTTPInteractionList w/ request matchers [method, uri] & 0 interaction(s): {  }
      [Cassette: "test"] - Initialized with options: {name: test, record: once, serialize_with: yaml, match_requests_on: c("method", "uri"), allow_playback_repeats: FALSE, preserve_exact_body_bytes: FALSE}
      [Cassette: "test"] - Ejecting

