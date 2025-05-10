#' Use casssettes in examples
#'
#' `insert_example_cassette()` is a wrapper around [insert_cassette()] that
#' stores casssettes in `inst/_vcr/`. Call it in the first line of your examples
#' (typically wrapped in `\dontshow{}`), and call `eject_cassette()` on the
#' last line.
#'
#' @export
#' @param package Package name.
#' @param record Record mode. This will be `"all"` if `package` is under
#'   development, (i.e. loaded by devtools) and `"none"` otherwise. These
#'   defaults ensure that you can easily re-record the cassette during
#'   development and the example will never make HTTP requests while
#'   testing.
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
  allow_playback_repeats = NULL,
  serialize_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL
) {
  dir <- example_cassette_path(package)
  if (is.null(dir)) {
    cli::cli_abort("`inst/_vcr` not found; please create.")
  }

  if (is.null(record)) {
    record <- if (is_dev_package(package) && !in_pkgdown()) "all" else "none"
  }

  insert_cassette(
    name = name,
    dir = dir,
    record = record,
    match_requests_on = match_requests_on,
    allow_playback_repeats = allow_playback_repeats,
    serialize_with = serialize_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes,
    re_record_interval = re_record_interval,
    clean_outdated_http_interactions = clean_outdated_http_interactions
  )
}

in_pkgdown <- function() {
  identical(Sys.getenv("IN_PKGDOWN"), "true")
}

example_cassette_path <- function(package) {
  path <- system_file("_vcr", package = package)
  if (path != "") {
    path
  } else if (is_dev_package(package)) {
    cli::cli_inform("Creating {.path inst/_vcr}")
    dir_create(file.path(system_file(package = package), "inst", "_vcr"))
  } else {
    NULL
  }
}

# partial system.file() shim that looks in the right place for packages
# loaded with devtools. pkgload takes care of this when calling system.file
# from the command line, but we need to implement from first principles inside
# a package.
system_file <- function(path = "", package, error_call = caller_env()) {
  if (is_dev_package(package)) {
    path_inst <- file.path(find.package(package), "inst", path)
    if (file.exists(path_inst)) {
      path_inst
    } else {
      ""
    }
  } else {
    system.file(path, package = package)
  }
}

is_dev_package <- function(name) {
  ns <- .getNamespace(name)
  !is.null(ns$.__DEVTOOLS__)
}
