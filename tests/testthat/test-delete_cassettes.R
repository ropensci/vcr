test_that("delete_cassettes() checks its inputs", {
  expect_snapshot(error = TRUE, {
    delete_cassettes()
    delete_cassettes("")
    delete_cassettes("test", type = "invalid")
  })
})

test_that("delete_cassettes() returns empty character when no cassettes found", {
  dir <- withr::local_tempdir()
  local_vcr_configure(dir = dir)

  expect_snapshot(
    {
      result <- delete_cassettes("nonexistent", type = "tests")
    },
    transform = function(x) gsub(dir, "<temp_dir>", x, fixed = TRUE)
  )
  expect_identical(result, character(0))
})

test_that("delete_cassettes() deletes cassettes with matching prefix", {
  dir <- withr::local_tempdir()
  local_vcr_configure(dir = dir)

  # Create some test cassettes
  local({
    local_cassette("api-get")
    httr::GET(hb("/get"))
  })

  local({
    local_cassette("api-post")
    httr::GET(hb("/get"))
  })

  local({
    local_cassette("other-test")
    httr::GET(hb("/get"))
  })

  # Verify cassettes were created
  expect_true(file.exists(file.path(dir, "api-get.yml")))
  expect_true(file.exists(file.path(dir, "api-post.yml")))
  expect_true(file.exists(file.path(dir, "other-test.yml")))

  # Delete cassettes with "api-" prefix
  expect_snapshot(
    {
      deleted <- delete_cassettes("api-", type = "tests")
    },
    transform = function(x) gsub(dir, "<temp_dir>", x, fixed = TRUE)
  )

  # Check that the right cassettes were deleted
  expect_length(deleted, 2)
  expect_all_true(grepl("api-", basename(deleted)))

  # Verify cassettes were actually deleted
  expect_false(file.exists(file.path(dir, "api-get.yml")))
  expect_false(file.exists(file.path(dir, "api-post.yml")))
  expect_true(file.exists(file.path(dir, "other-test.yml")))
})

test_that("delete_cassettes() works with exact cassette name", {
  dir <- withr::local_tempdir()
  local_vcr_configure(dir = dir)

  # Create test cassettes
  local({
    local_cassette("exact")
    httr::GET(hb("/get"))
  })

  local({
    local_cassette("exact-2")
    httr::GET(hb("/get"))
  })

  # Verify cassettes were created
  expect_true(file.exists(file.path(dir, "exact.yml")))
  expect_true(file.exists(file.path(dir, "exact-2.yml")))

  # Delete with exact name will also delete exact-2
  expect_snapshot(
    {
      deleted <- delete_cassettes("exact", type = "tests")
    },
    transform = function(x) gsub(dir, "<temp_dir>", x, fixed = TRUE)
  )

  expect_length(deleted, 2)
  expect_false(file.exists(file.path(dir, "exact.yml")))
  expect_false(file.exists(file.path(dir, "exact-2.yml")))
})

test_that("delete_cassettes() works with multiple types", {
  # Set up directories
  test_dir <- withr::local_tempdir()
  pkg_root <- withr::local_tempdir()
  dir_create(file.path(pkg_root, "inst", "_vcr"))
  dir_create(file.path(pkg_root, "vignettes", "_vcr"))

  local_vcr_configure(dir = test_dir)

  # Create cassettes in test directory
  local({
    local_cassette("multi-test")
    httr::GET(hb("/get"))
  })

  # Create cassettes in inst/_vcr
  inst_cassette <- file.path(pkg_root, "inst", "_vcr", "multi-example.yml")
  file.create(inst_cassette)
  writeLines("test content", inst_cassette)

  # Create cassettes in vignettes/_vcr
  vig_cassette <- file.path(pkg_root, "vignettes", "_vcr", "multi-vignette.yml")
  file.create(vig_cassette)
  writeLines("test content", vig_cassette)

  # Verify cassettes exist
  expect_true(file.exists(file.path(test_dir, "multi-test.yml")))
  expect_true(file.exists(inst_cassette))
  expect_true(file.exists(vig_cassette))

  # Mock rprojroot to return our test package root
  local_mocked_bindings(
    find_package_root_file = function() pkg_root,
    .package = "rprojroot"
  )

  # Delete cassettes with "multi-" prefix from multiple locations
  expect_snapshot(
    {
      deleted <- delete_cassettes(
        "multi-",
        type = c("tests", "examples", "vignettes")
      )
    },
    transform = function(x) {
      x <- gsub(test_dir, "<test_dir>", x, fixed = TRUE)
      x <- gsub(
        file.path(pkg_root, "inst", "_vcr"),
        "<inst_vcr>",
        x,
        fixed = TRUE
      )
      x <- gsub(
        file.path(pkg_root, "vignettes", "_vcr"),
        "<vignettes_vcr>",
        x,
        fixed = TRUE
      )
      x
    }
  )

  expect_length(deleted, 3)
  expect_false(file.exists(file.path(test_dir, "multi-test.yml")))
  expect_false(file.exists(inst_cassette))
  expect_false(file.exists(vig_cassette))
})

test_that("delete_cassettes() handles different serialization formats", {
  dir <- withr::local_tempdir()
  local_vcr_configure(dir = dir)

  # Create cassettes with different formats
  local({
    local_cassette("format-test1", serialize_with = "yaml")
    httr::GET(hb("/get"))
  })

  local({
    local_cassette("format-test2", serialize_with = "json")
    httr::GET(hb("/get"))
  })

  # Manually create a .qs2 file for testing
  qs2_file <- file.path(dir, "format-test3.qs2")
  file.create(qs2_file)

  # Verify files exist
  expect_true(file.exists(file.path(dir, "format-test1.yml")))
  expect_true(file.exists(file.path(dir, "format-test2.json")))
  expect_true(file.exists(qs2_file))

  # Delete all with "format-" prefix
  expect_snapshot(
    {
      deleted <- delete_cassettes("format-", type = "tests")
    },
    transform = function(x) gsub(dir, "<temp_dir>", x, fixed = TRUE)
  )

  expect_length(deleted, 3)
  expect_false(file.exists(file.path(dir, "format-test1.yml")))
  expect_false(file.exists(file.path(dir, "format-test2.json")))
  expect_false(file.exists(qs2_file))
})

test_that("delete_cassettes() informs when directory doesn't exist", {
  pkg_root <- withr::local_tempdir()
  # Don't create the inst/_vcr directory

  local_mocked_bindings(
    find_package_root_file = function() pkg_root,
    .package = "rprojroot"
  )

  expect_snapshot(
    {
      result <- delete_cassettes("test", type = "examples")
    },
    transform = function(x) {
      gsub(file.path(pkg_root, "inst", "_vcr"), "<inst_vcr>", x, fixed = TRUE)
    }
  )
  expect_identical(result, character(0))
})

test_that("delete_cassettes() informs when no cassettes match prefix", {
  dir <- withr::local_tempdir()
  local_vcr_configure(dir = dir)

  # Create a cassette with different prefix
  local({
    local_cassette("other")
    httr::GET(hb("/get"))
  })

  expect_snapshot(
    {
      result <- delete_cassettes("nomatch", type = "tests")
    },
    transform = function(x) gsub(dir, "<temp_dir>", x, fixed = TRUE)
  )
  expect_identical(result, character(0))

  # Original cassette should still exist
  expect_true(file.exists(file.path(dir, "other.yml")))
})

test_that("delete_cassettes() respects vcr_configure() directory", {
  custom_dir <- withr::local_tempdir()
  local_vcr_configure(dir = custom_dir)

  # Create a cassette in the custom directory
  local({
    local_cassette("configured-test")
    httr::GET(hb("/get"))
  })

  # Verify cassette was created in custom directory
  expect_true(file.exists(file.path(custom_dir, "configured-test.yml")))

  # Delete should find and delete from the configured directory
  expect_snapshot(
    {
      deleted <- delete_cassettes("configured-", type = "tests")
    },
    transform = function(x) gsub(custom_dir, "<custom_dir>", x, fixed = TRUE)
  )

  expect_length(deleted, 1)
  expect_false(file.exists(file.path(custom_dir, "configured-test.yml")))
})
