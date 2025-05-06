# useful error if not registered

    Code
      serializer_fetch("foo")
    Condition
      Error in `serializer_fetch()`:
      ! Unsupported cassette serializer "foo".

# generates expected yaml

    Code
      writeLines(readLines(ser$path))
    Output
      {
        "http_interactions": [
          {
            "request": {
              "method": "GET",
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
            "recorded_at": "2024-01-01 12:00:00"
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
          method: GET
          uri: http://example.com
          body: {}
          headers: []
        response:
          status: 200
          headers:
            name: val
          body:
            string: body
        recorded_at: 2024-01-01 12:00:00
      recorded_with: <package_versions>

