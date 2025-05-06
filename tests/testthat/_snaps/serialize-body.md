# warns about v1 bodies with a string that's base64

    Code
      out <- decode_body(body, preserve_bytes = TRUE)
    Condition
      Warning:
      "test" cassette uses outdated encoding. Please rerecord it.

