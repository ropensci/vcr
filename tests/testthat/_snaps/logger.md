# vcr_log_sprintf() adds additional metadata

    Code
      vcr_log_sprintf("log")
    Output
      [Cassette: <none>] - log

---

    Code
      vcr_log_sprintf("log")
    Output
      2023-12-31 18:00:00 - [Cassette: "testing"] - log

