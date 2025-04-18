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

# cassette warns if ejected with no interactions

    Code
      . <- cl$eject()
    Condition
      Warning:
      Empty cassette (test) deleted; consider the following:
       - If an error occurred resolve that first, then check:
       - vcr only supports crul, httr & httr2; requests w/ curl, download.file, etc. are not supported
       - If you are using crul/httr/httr2, are you sure you made an HTTP request?

