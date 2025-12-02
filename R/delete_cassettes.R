#' Delete cassettes by prefix
#'
#' @description
#' Delete cassettes that match a given prefix from the specified directory
#' (tests, examples, or vignettes). This is useful for cleaning up old or
#' unwanted cassettes in batch.
#'
#' @export
#' @param prefix (character) The prefix to match cassette names. This will
#'   match cassette names that start with this string. For example, `prefix = "api"`
#'   will match cassettes like `"api-get.yml"`, `"api-post.yml"`, etc. To match
#'   a single cassette exactly, include the full name without the extension
#'   (e.g., `prefix = "my-cassette"` will match `"my-cassette.yml"` and
#'   `"my-cassette-2.yml"`).
#' @param type (character) The type(s) of cassettes to delete. Can be one or more of:
#'
#'   * **tests** (default): Delete cassettes from the directory configured with
#'     [vcr_configure()]. If not configured, defaults to `tests/testthat/_vcr/`.
#'   * **examples**: Delete cassettes in `inst/_vcr/`
#'   * **vignettes**: Delete cassettes in `vignettes/_vcr/`
#'
#' @return A character vector of the deleted cassette paths (invisibly). If no
#'   cassettes match the prefix, returns `character(0)`.
#'
#' @details
#' The function will:
#' 1. Look for cassettes (`.yml`, `.yaml`, `.json`, or `.qs2` extensions)
#'    in the specified directory or directories.
#' 2. Match cassettes whose names start with the given prefix.
#' 3. Delete all matching cassettes.
#' 4. Report how many cassettes were deleted.
#'
#' @seealso [cassette_path()] for locating cassettes, [use_cassette()] for
#'   creating cassettes.
#'
#' @examples
#' \dontrun{
#' # Delete all test cassettes starting with "api-"
#' delete_cassettes("api-", type = "tests")
#'
#' # Delete all example cassettes starting with "github"
#' delete_cassettes("github", type = "examples")
#'
#' # Delete a specific vignette cassette (and any cassettes starting with that name)
#' delete_cassettes("intro-example", type = "vignettes")
#'
#' # Delete cassettes from multiple locations
#' delete_cassettes("old-", type = c("tests", "examples"))
#' }
delete_cassettes <- function(
  prefix,
  type = c("tests", "examples", "vignettes")
) {
  check_string(prefix, allow_empty = FALSE)
  type <- arg_match(type, multiple = TRUE)

  all_deleted <- map(type, delete_type_cassettes, prefix = prefix)

  all_deleted <- unlist(all_deleted)

  if (length(all_deleted) == 0) {
    cli::cli_inform(
      "No cassettes matching prefix {.str {prefix}} were found."
    )
    return(invisible(character(0)))
  }

  cli::cli_alert_success(
    "Deleted {length(all_deleted)} cassette{?s} matching prefix {.str {prefix}}:"
  )
  walk(all_deleted, function(cassette) {
    cli::cli_bullets(c("*" = "{.file {basename(cassette)}}"))
  })

  invisible(all_deleted)
}

delete_type_cassettes <- function(type, prefix) {
  dir <- switch(
    type,
    tests = cassette_path(),
    examples = file.path(rprojroot::find_package_root_file(), "inst", "_vcr"),
    vignettes = file.path(
      rprojroot::find_package_root_file(),
      "vignettes",
      "_vcr"
    )
  )

  if (!dir.exists(dir)) {
    cli::cli_inform(
      "Directory {.path {dir}} does not exist. Nothing to delete."
    )
    return(character(0))
  }

  # Get cassettes matching the prefix and extensions
  matching_cassettes <- list.files(
    dir,
    pattern = paste0("^", prefix, ".*\\.(yml|yaml|json|qs2)$"),
    full.names = TRUE
  )

  if (length(matching_cassettes) == 0) {
    cli::cli_inform(
      "No cassettes matching prefix {.str {prefix}} found in {.path {dir}}."
    )
    return(character(0))
  }

  # Delete the matching cassettes
  deleted <- keep(matching_cassettes, \(x) {
    success <- file.remove(x)
    if (!success) {
      cli::cli_warn("Failed to delete {.path {x}}.")
    }
    success
  })

  deleted
}
