# helpful error if no requests match

    Code
      use_cassette("test", httr2::req_perform(req))
    Condition
      Error:
      ! Failed to find matching request in active cassette.
      i Use `local_vcr_configure_log()` to get more details.
      i Learn more in `vignette(vcr::debugging)`.

