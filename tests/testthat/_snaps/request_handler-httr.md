# fails well if write_disk_path not set

    Code
      use_cassette("test", httr::GET(hb("/get"), httr::write_disk(path, TRUE)))
    Condition
      Error in `save_file()`:
      ! `write_disk_path` must be set when writing to disk.
      i See ?vcr_configure for details.

