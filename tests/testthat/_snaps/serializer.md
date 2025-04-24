# useful error if not registered

    Code
      serializer_fetch("foo")
    Condition
      Error in `serializer_fetch()`:
      ! Unsupported cassette serializer "foo".

# warns if you reload string with preserve_exact_body_bytes

    Code
      use_cassette("test", httr::GET(hb("/get")), preserve_exact_body_bytes = TRUE)
    Condition
      Warning in `decode_body()`:
      re-record cassettes using 'preserve_exact_body_bytes = TRUE'

# generates expected yaml

    Code
      writeLines(readLines(ser$path))
    Output
      {
        "http_interactions": [
          {
            "request": {
              "method": "get",
              "uri": "http://example.com",
              "body": {},
              "headers": []
            },
            "response": {
              "status": 200,
              "headers": {
                "name": "val"
              },
              "body": {
                "string": "body"
              }
            },
            "recorded_at": "2024-01-01 12:00:00 GMT"
          }
        ],
        "recorded_with": "<package_versions>"
      }

---

    Code
      writeLines(readLines(ser$path))
    Output
      http_interactions:
      - request:
          method: get
          uri: http://example.com
          body: {}
          headers: []
        response:
          status: 200
          headers:
            name: val
          body:
            string: body
        recorded_at: 2024-01-01 12:00:00 GMT
      recorded_with: <package_versions>
