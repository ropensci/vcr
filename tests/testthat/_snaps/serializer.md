# useful error if not registered

    Code
      serializer_fetch("foo")
    Condition
      Error in `serializer_fetch()`:
      ! Unsupported cassette serializer "foo".

# generates expected json

    Code
      writeLines(readLines(ser$path))
    Output
      {
        "http_interactions": [
          {
            "request": {
              "method": "get",
              "uri": "http://example.com",
              "body": {
                "encoding": "",
                "string": ""
              },
              "headers": []
            },
            "response": {
              "status": 200,
              "headers": {
                "name": "val"
              },
              "body": {
                "encoding": "",
                "string": "body"
              }
            },
            "recorded_at": "2024-01-01 12:00:00 GMT"
          }
        ],
        "recorded_with": "<package_versions>"
      }

# generates expected yaml

    Code
      writeLines(readLines(ser$path))
    Output
      http_interactions:
      - request:
          method: get
          uri: http://example.com
          body:
            encoding: ''
            string: ''
          headers: []
        response:
          status: 200
          headers:
            name: val
          body:
            encoding: ''
            string: body
        recorded_at: 2024-01-01 12:00:00 GMT
      recorded_with: <package_versions>

# generates expected compressed data

    Code
      writeLines(qs2::qs_read(ser$path))
    Output
      {"http_interactions":[{"request":{"method":"get","uri":"http://example.com","body":{"encoding":"","string":""},"headers":[]},"response":{"status":200,"headers":{"name":"val"},"body":{"encoding":"","string":"body"}},"recorded_at":"2024-01-01 12:00:00 GMT"}],"recorded_with":"<package_versions>"}

