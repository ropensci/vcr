# useful error with bad serializer

    Code
      Cassette$new("newbar6", serialize_with = "howdy")
    Condition
      Error in `serializer_fetch()`:
      ! Unsupported cassette serializer "howdy".

# cassette checks name

    Code
      Cassette$new("foo bar")
    Condition
      Error in `initialize()`:
      ! `name` must not contain spaces.
    Code
      Cassette$new("foo.yml")
    Condition
      Error in `initialize()`:
      ! `name` must not include an extension.
    Code
      Cassette$new("foo/bar")
    Condition
      Error in `initialize()`:
      ! `name` must not contain '/', '?', '<', '>', '\', ':', '*', '|', or '"'
    Code
      Cassette$new("foo\nbar")
    Condition
      Error in `initialize()`:
      ! `name` must not contain spaces.
    Code
      Cassette$new("foo\nbar.")
    Condition
      Error in `initialize()`:
      ! `name` must not contain spaces.
    Code
      Cassette$new("..")
    Condition
      Error in `initialize()`:
      ! `name` must not be '.', '..', etc.
    Code
      Cassette$new("con")
    Condition
      Error in `initialize()`:
      ! `name` must not contain reserved windows strings.
    Code
      Cassette$new(strrep("x", 400))
    Condition
      Error in `initialize()`:
      ! `name` must be less than 256 characters.

---

    Code
      Cassette$new("foo")
    Condition
      Error in `initialize()`:
      ! `name` must not be the same as an existing cassette.

