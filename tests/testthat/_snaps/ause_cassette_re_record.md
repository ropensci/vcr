# use_cassette options: re_record_interval

    Code
      use_cassette("test", res <- conn$get("get"))
    Output
      [Cassette: test] Inserting 'test.yml' (with 1 interactions)
      [Cassette: test] Removing 1 outdated interactions and re-recording.
      [Cassette: test]   Mode: recording and replaying
      [Cassette: test] Handling request: GET {httpbin}/get
      [Cassette: test]   Looking for existing requests using method/uri
      [Cassette: test]   No matching requests
      [Cassette: test]   Recording response: 200 with {bytes} bytes of application/json data
      [Cassette: test] Ejecting

