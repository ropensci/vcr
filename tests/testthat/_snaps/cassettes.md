# ejecting errors if no cassettes

    Code
      eject_cassette()
    Condition
      Error in `eject_cassette()`:
      ! No cassette in use.

# inserting a cassette errors when vcr turned off and ignore_cassettes=FALSE

    Code
      insert_cassette("test")
    Condition
      Error in `insert_cassette()`:
      ! vcr is turned off.
      i Use `turn_on()` to turn it back on.
      i Or use `turn_off(ignore_cassettes = TRUE)` to cassettes completely.

