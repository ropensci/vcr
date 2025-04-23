# filter_sensitive data strips quotes with message

    Code
      vcr_configure(filter_sensitive_data = list(key = "\"val\""))
    Condition
      Warning:
      filter_sensitive_data: leading & trailing quotes trimmed from 'key'

