# use_cassette() checks its inputs

    Code
      use_cassette()
    Condition
      Error in `use_cassette()`:
      ! `name` is absent but must be supplied.
    Code
      use_cassette("test")
    Condition
      Error in `use_cassette()`:
      ! `...` must not be empty.
      i Do you want `local_cassette()` instead?
    Code
      use_cassette("test", NULL, record = "xxx")
    Condition
      Error:
      ! 'record' value of 'xxx' is not in the allowed set: none, once, new_episodes, all
    Code
      use_cassette("test", NULL, match_requests_on = "xxx")
    Condition
      Error:
      ! 1 or more 'match_requests_on' values (xxx) is not in the allowed set: method, uri, headers, host, path, body, body_json, query
    Code
      use_cassette("text", NULL, preserve_exact_body_bytes = "xxx")
    Condition
      Error:
      ! preserve_exact_body_bytes must be of class logical

