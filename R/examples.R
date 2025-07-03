#' Use cassettes in examples
#'
#' @description
#' `insert_example_cassette()` is a wrapper around [insert_cassette()] that
#' stores cassettes in `inst/_vcr/`. Call it in the first line of your examples
#' (typically wrapped in `\dontshow{}`), and call `eject_cassette()` on the
#' last line.
#'
#' Run the example manually once to record the vignettte, then it will be
#' replayed during `R CMD check`, ensuring that your example no longer uses
#' the internet.
#'
#' @export
#' @param package Package name.
#' @param record Record mode. This will be `"once"` if `package` is under
#'   development, (i.e. loaded by devtools) and `"none"` otherwise. This makes
#'   it easy to record during development and ensure that cassettes HTTP
#'   requests are never made on CRAN.
#'
#'   To re-record all cassettes, you can delete `inst/_vcr` then run
#'   `pkgdown::build_reference(lazy = FALSE)`.
#' @inheritParams insert_cassette
#' @examplesIf requireNamespace("httr2")
#' # In this example I'm showing the insert and eject commands, but you'd
#' # usually wrap these in \dontshow{} so the user doesn't see them and
#' # think that they're something they need to copy.
#'
#' insert_example_cassette("httpbin-get", package = "vcr")
#'
#' req <- httr2::request("https://hb.cran.dev/get")
#' resp <- httr2::req_perform(req)
#'
#' str(httr2::resp_body_json(resp))
#'
#' eject_cassette()
insert_example_cassette <- function(
  name,
  package,
  record = NULL,
  match_requests_on = NULL,
  serialize_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL
) {
  dir <- example_cassette_path(package)
  in_dev <- is_dev_package(package)
  if (is.null(record)) {
    record <- if (in_dev) "once" else "none"
  }

  cassette <- insert_cassette(
    name = name,
    dir = dir,
    record = record,
    match_requests_on = match_requests_on,
    serialize_with = serialize_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes,
    re_record_interval = re_record_interval
  )

  path <- cassette$file()
  if (record == "none" && !in_dev && !file.exists(path)) {
    cli::cli_abort(
      c(
        "Can't find pre-recorded cassette {.path inst/_vcr/{basename(path)}}.",
        i = "Do you need to run {.fn insert_example_cassette} in a live session?"
      )
    )
  }

  invisible(cassette)
}

example_cassette_path <- function(package, call = caller_env()) {
  if (is_dev_package(package)) {
    path <- file.path(find.package(package), "inst", "_vcr")
    if (!dir.exists(path)) {
      cli::cli_inform("Creating {.path inst/_vcr}.")
      dir_create(path)
    }
  } else {
    path <- system.file("_vcr", package = package)
    if (path == "") {
      cli::cli_abort(
        c(
          "Can't find {.path inst/_vcr} directory.",
          i = "Do you need to run {.fn insert_example_cassette} in a live session?"
        ),
        call = call
      )
    }
  }

  path
}

in_pkgdown <- function() {
  identical(Sys.getenv("IN_PKGDOWN"), "true")
}

is_dev_package <- function(name) {
  ns <- .getNamespace(name)
  !is.null(ns$.__DEVTOOLS__)
}
