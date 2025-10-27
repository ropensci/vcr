make_pkg_path <- function(base_dir = tempdir(), pkg_name = "demo") {

  d <- file.path(base_dir, pkg_name)
  if (!dir.exists(d)) {
        dir.create(d, recursive = TRUE)
    }

    return (d)
}

get_roxygen_version <- function() {

  ip <- as.data.frame(utils::installed.packages ())
  if (!"roxygen2" %in% ip$Package) {
      return(NULL)
  } # nocov

  return(ip$Version[ip$Package == "roxygen2"])
}

write_desc <- function(d, pkg_name) {

  desc <- c (
    paste0 ("Package: ", pkg_name),
    "Title: What the Package Does (One Line, Title Case)",
    "Version: 0.0.0.9000",
    "Authors@R: ",
    "  person(given = \"First\",",
    "         family = \"Last\",",
    "         role = c(\"aut\", \"cre\"),",
    "         email = \"first.last@example.com\")",
    "Description: What the package does (one paragraph).",
    "Suggests:",
    "    testthat",
    "License: GPL-3",
    "Encoding: UTF-8"
  )

  rv <- get_roxygen_version()
  if (!is.null (rv)) {

    desc <- c(
      desc,
      paste0 (
        "Roxygen: list(markdown = TRUE, ",
        "roclets = c(\"collate\", \"rd\", \"namespace\", \"vcr::examplesVCR_roclet\"))"
      )
    )
  }

  writeLines(desc, con = file.path (d, "DESCRIPTION"))
}

write_r_fn <- function(d, pkg_name) {

  rfile <- c (
      "#' test_fn",
      "#'",
      "#' A test funtion",
      "#'",
      "#' @examplesVCR test_fn",
      "#' test_fn",
      "#'",
      "#' @export",
      "test_fn <- function() {",
      "  message(\"This function does nothing\")",
      "}"
  )
  dr <- file.path (d, "R")
  if (!dir.exists(dr)) {
      dir.create (dr, recursive = TRUE)
  }
  writeLines(rfile, con = file.path(dr, "test.R"))

  rfile <- c(
      "#' @keywords internal",
      "\"_PACKAGE\"",
      "",
      paste0 (
        "# The following block is used by ",
        "usethis to automatically manage"
      ),
      "# roxygen namespace tags. Modify with care!",
      "## usethis namespace: start",
      "## usethis namespace: end",
      "NULL"
  )
  writeLines(rfile, con = file.path(dr, paste0(pkg_name, "-package.R")))
}

#' Make skeleton package to test roclet system
#'
#' @param base_dir The base directory where the package should be constructed.
#' @param pkg_name The name of the package. The final location of this package
#' will be in `file.path(base_dir, pkg_name)`.
#' @return The path to the directory holding the newly created package
#' @family helper
#' @export
vcr_tests_pkg_skeleton <- function(base_dir = tempdir(), pkg_name = "demo") {

    d <- make_pkg_path(base_dir, pkg_name)

    if (length(list.files(d, recursive = TRUE)) > 0L) {
        stop (
            "The path [", d, "] is not empty; ",
            "can only make a package in an empty directory\n",
            "  Directory can be cleared with ",
            "'unlink(<dir>, recursive = TRUE)'"
        )
    }

    write_desc(d, pkg_name)
    write_r_fn(d, pkg_name)

    return(d)
}
