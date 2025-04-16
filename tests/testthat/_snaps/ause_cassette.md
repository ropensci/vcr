# use_cassette fails well

    Code
      use_cassette()
    Condition
      Error in `use_cassette()`:
      ! `name` is absent but must be supplied.

---

    Code
      use_cassette("foobar333")
    Condition
      Error in `use_cassette()`:
      ! `...` must not be empty.
      i Do you want `local_cassette()` instead?

