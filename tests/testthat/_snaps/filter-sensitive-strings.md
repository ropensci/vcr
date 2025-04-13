# filter sensitive data strips leading/trailing single/double quotes

    Code
      cas <- use_cassette("testing2", res <- x$get("get", query = list(key = Sys.getenv(
        "MY_KEY_ON_GH_ACTIONS"))))
    Condition
      Warning:
      filter_sensitive_data: leading & trailing quotes trimmed from '<somekey>'

