# local_cassette() checks its inputs

    Code
      local_cassette()
    Condition
      Error in `local_cassette()`:
      ! `name` must be a single string, not absent.
    Code
      local_cassette("test")
      local_cassette("test", NULL, record = "xxx")
    Condition
      Error in `local_cassette()`:
      ! `record` must be one of "none", "once", "new_episodes", or "all", not "xxx".
    Code
      local_cassette("test", NULL, match_requests_on = "xxx")
    Condition
      Error in `local_cassette()`:
      ! `match_requests_on` must be one of "method", "uri", "headers", "host", "path", "body", "body_json", or "query", not "xxx".
    Code
      local_cassette("text", NULL, preserve_exact_body_bytes = "xxx")
    Condition
      Error in `local_cassette()`:
      ! `preserve_exact_body_bytes` must be `TRUE`, `FALSE`, or `NULL`, not the string "xxx".
    Code
      local_cassette("test", NULL, serialize_with = "howdy")
    Condition
      Error in `serializer_fetch()`:
      ! Unsupported cassette serializer "howdy".

# local_cassette checks name

    Code
      local_cassette("foo bar")
    Condition
      Error in `local_cassette()`:
      ! `name`, "foo bar", must not contain spaces.
    Code
      local_cassette("foo.yml")
    Condition
      Error in `local_cassette()`:
      ! `name`, "foo.yml", must not include an extension.
    Code
      local_cassette("foo/bar")
    Condition
      Error in `local_cassette()`:
      ! `name` must not contain '/', '?', '<', '>', '\', ':', '*', '|', or '"'
    Code
      local_cassette("foo\nbar")
    Condition
      Error in `local_cassette()`:
      ! `name`, "foo bar", must not contain spaces.
    Code
      local_cassette("foo\nbar.")
    Condition
      Error in `local_cassette()`:
      ! `name`, "foo bar.", must not contain spaces.
    Code
      local_cassette("..")
    Condition
      Error in `local_cassette()`:
      ! `name` must not be '.', '..', etc.
    Code
      local_cassette("con")
    Condition
      Error in `local_cassette()`:
      ! `name` must not contain reserved windows strings.
    Code
      local_cassette(strrep("x", 400))
    Condition
      Error in `local_cassette()`:
      ! `name` must be less than 256 characters.

---

    Code
      local({
        local_cassette("foo")
        local_cassette("foo")
      })
    Condition
      Error in `local_cassette()`:
      ! `name`, "foo", must not be the same as an existing cassette.

