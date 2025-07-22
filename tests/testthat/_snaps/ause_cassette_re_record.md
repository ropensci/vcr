# use_cassette options: re_record_interval

    Code
      use_cassette("testing1", res <- conn$get("get"))
    Output
      [Cassette: testing1] Inserting 'testing1.yml' (with 1 interactions)
      [Cassette: testing1] Removing 1 outdated interactions and re-recording.
      [Cassette: testing1]   Mode: recording and replaying
      [Cassette: testing1] Handling request: GET {httpbin}/get
      [Cassette: testing1]   Looking for existing requests using method/uri
      [Cassette: testing1]   No matching requests
      [Cassette: testing1]   Recording response: 200 with {bytes} bytes of application/json data
      [Cassette: testing1] Ejecting

