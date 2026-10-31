# streaming requests that aren't in the cassette error when replaying

    Code
      use_cassette("test", httr2::req_perform_connection(httr2::request(hb(
        "/stream/2"))), record = "none")
    Condition
      Error:
      ! Failed to find matching request in active cassette, "test".
      i Use `local_vcr_configure_log()` to get more details.
      i Learn more in `vignette(vcr::debugging)`.

