# config checks inputs

    Code
      vcr_configure(record = "asdfadfs")
    Condition
      Error:
      ! `value` must be one of "none", "once", "new_episodes", or "all", not "asdfadfs".
    Code
      vcr_configure(match_requests_on = "x")
    Condition
      Error:
      ! `value` must be one of "method", "uri", "headers", "host", "path", "body", "body_json", or "query", not "x".

# filter_sensitive data strips quotes with message

    Code
      vcr_configure(filter_sensitive_data = list(key = "\"val\""))
    Condition
      Warning:
      filter_sensitive_data: leading & trailing quotes trimmed from 'key'

