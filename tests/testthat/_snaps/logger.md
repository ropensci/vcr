# vcr_log_sprintf() adds additional metadata

    Code
      vcr_log_sprintf("log")
    Output
      [Cassette: <none>] log

---

    Code
      vcr_log_sprintf("log")
    Output
      2024-01-01 12:00:00 [Cassette: testing] log

