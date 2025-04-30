# UnhandledHTTPRequestError fails well

    Code
      UnhandledHTTPRequestError$new(5)
    Condition
      Error in `check_vcr_request()`:
      ! `request` must be a <vcr_request>, not the number 5.

# informative error if no cassette active

    Code
      err$construct_message()
    Condition
      Error:
      ! ================================================================================
      An HTTP request has been made that vcr does not know how to handle:
      POST http://example.com
      There is currently no cassette in use.
      
      Run `vcr::vcr_last_error()` for more verbose errors
      If you're not sure what to do, open an issue https://github.com/ropensci/vcr/issues
      & see https://books.ropensci.org/http-testing
      ================================================================================

# UnhandledHTTPRequestError works as expected

    Code
      err$construct_message()
    Condition
      Error:
      ! ================================================================================
      An HTTP request has been made that vcr does not know how to handle:
      POST http://example.com
      vcr is currently using the following cassette:
        - _vcr/turtle.yml
          - record_mode: once
          - match_requests_on: method, uri
      Run `vcr::vcr_last_error()` for more verbose errors
      If you're not sure what to do, open an issue https://github.com/ropensci/vcr/issues
      & see https://books.ropensci.org/http-testing
      ================================================================================

