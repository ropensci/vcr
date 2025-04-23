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
        recorded_at: 2024-01-01 00:00:00 GMT
        recorded_with: <package_versions>

