# delete_cassettes() checks its inputs

    Code
      delete_cassettes()
    Condition
      Error in `delete_cassettes()`:
      ! `prefix` must be a single string, not absent.
    Code
      delete_cassettes("")
    Condition
      Error in `delete_cassettes()`:
      ! `prefix` must be a single string, not the empty string "".
    Code
      delete_cassettes("test", type = "invalid")
    Condition
      Error in `delete_cassettes()`:
      ! `type` must be one of "tests", "examples", or "vignettes", not "invalid".

# delete_cassettes() returns empty character when no cassettes found

    Code
      result <- delete_cassettes("nonexistent", type = "tests")
    Message
      No cassettes matching prefix "nonexistent" found in '<temp_dir>'.
      No cassettes matching prefix "nonexistent" were found.

# delete_cassettes() deletes cassettes with matching prefix

    Code
      deleted <- delete_cassettes("api-", type = "tests")
    Message
      v Deleted 2 cassettes matching prefix "api-":
      * 'api-get.yml'
      * 'api-post.yml'

# delete_cassettes() works with multiple types

    Code
      deleted <- delete_cassettes("multi-", type = c("tests", "examples", "vignettes"))
    Message
      v Deleted 3 cassettes matching prefix "multi-":
      * 'multi-test.yml'
      * 'multi-example.yml'
      * 'multi-vignette.yml'

# delete_cassettes() handles different serialization formats

    Code
      deleted <- delete_cassettes("format-", type = "tests")
    Message
      v Deleted 3 cassettes matching prefix "format-":
      * 'format-test1.yml'
      * 'format-test2.json'
      * 'format-test3.qs2'

# delete_cassettes() informs when directory doesn't exist

    Code
      result <- delete_cassettes("test", type = "examples")
    Message
      Directory '<inst_vcr>' does not exist. Nothing to delete.
      No cassettes matching prefix "test" were found.

# delete_cassettes() informs when no cassettes match prefix

    Code
      result <- delete_cassettes("nomatch", type = "tests")
    Message
      No cassettes matching prefix "nomatch" found in '<temp_dir>'.
      No cassettes matching prefix "nomatch" were found.

