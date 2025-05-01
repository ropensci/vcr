# config checks inputs

    Code
      vcr_configure(foo = "bar")
    Condition
      Error in `vcr_configure()`:
      ! unused argument (foo = "bar")
    Code
      vcr_configure(record = "asdfadfs")
    Condition
      Error:
      ! 'record' value of 'asdfadfs' is not in the allowed set: none, once, new_episodes, all
    Code
      vcr_configure(match_requests_on = "x")
    Condition
      Error:
      ! 1 or more 'match_requests_on' values (x) is not in the allowed set: method, uri, headers, host, path, body, query

# filter_sensitive data strips quotes with message

    Code
      vcr_configure(filter_sensitive_data = list(key = "\"val\""))
    Condition
      Warning:
      filter_sensitive_data: leading & trailing quotes trimmed from 'key'

