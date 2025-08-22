roxy_tag_rd.roxy_tag_examplesVCR <- function(x, base_path, env) {
  roxygen2::rd_section("examples", x$val)
}

roxy_tag_parse.roxy_tag_examplesVCR <- function(x, ...) {
  lines <- unlist(strsplit(x$raw, "\r?\n"))

  cassette_name <- trimws(lines[1])
  package_name <- roxygen2::roxy_meta_get("current_package")

  x$raw <- paste(
    c(
      sprintf(
        "\\dontshow{vcr::insert_example_cassette('%s', package = '%s')}",
        cassette_name,
        package_name
      ),
      lines[-1],
      "\\dontshow{vcr::eject_cassette()}"
    ),
    collapse = "\n"
  )
  x$value <- ""
  roxygen2::tag_examples(x)
}


#' @export
#' @keywords internal
#' @title Roclet for VCR examples
# https://github.com/shahronak47/informationtag
examplesVCR_roclet <- function() {
  roxygen2::roclet("examplesVCR")
}

roclet_process.roclet_examplesVCR <- function(x, blocks, env, base_path) {
  x
}

roclet_output.roclet_examplesVCR <- function(x, results, base_path, ...) {
  x
}

#' Use 'vcr' for examples in your package
#'
#' This functions amends DESCRIPTION to set up vcr's roxygen2 machinery.
#' Call it once when you're setting a new package.
#'
#' @param path character path to the package
#'
#' @return Nothing: called for file system side effects.
#' @export
#'
use_vcr_examples <- function(path = ".") {
  rlang::check_installed("desc")

  cli::cli_alert_info("Registering vcr's roxygen2 tag usage in DESCRIPTION")

  current_roxy <- desc::desc_get("Roxygen", file = path)[[1]]
  if (is.na(current_roxy)) {
    desc::desc_set(
      "Roxygen",
      'list(markdown = TRUE, roclets = c("collate", "rd", "namespace", "vcr::examplesVCR_roclet", packages = "vcr"))'
    )
  } else {
    current <- eval(parse(text = current_roxy))
    new <- current
    new[["roclets"]] <- union(
      current[["roclets"]],
      c("collate", "rd", "namespace", "vcr::examplesVCR_roclet")
    )
    new[["packages"]] <- union(current[["packages"]], "vcr")

    new_string <- paste(deparse(new), collapse = "")
    desc::desc_set("Roxygen", new_string, file = path)
  }

  cli::cli_alert_info("Registering vcr build-time dependency in DESCRIPTION")
  desc::desc_set(
    "Config/Needs/build",
    paste_desc(
      desc::desc_get("Config/Needs/build", file = path),
      "vcr"
    ),
    file = path
  )
}

#' Append value if needed
#'
#' @param x Existing DESCRIPTION field value
#' @param y Value to append
#'
#' @return A character string
#' @noRd
#'
#' @examples
#' paste_desc(NA, 1)
#' paste_desc(1, 1)
#' paste_desc(2, 1)
paste_desc <- function(x, y) {
  if (is.na(x)) {
    return(y)
  }

  # ensure this is idempotent
  toString(unique(c(x, y)))
}
