vcr_text <- list(
  test_all = "library(\"testthat\")
test_check(\"%s\")\n",
  config = "library(\"vcr\") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path(\"fixtures\")
))\nvcr::check_cassette_names()\n",
  example_test = "# EXAMPLE VCR USAGE: RUN AND DELETE ME

foo <- function() crul::ok('https://hb.opencpu.org/get')

test_that(\"foo works\", {
  vcr::use_cassette(\"testing\", {
    x <- foo()
  })
  expect_true(x)
})\n",
  learn_more = "Learn more about `vcr`: https://books.ropensci.org/http-testing",
  gitattributes = "* text=auto
tests/fixtures/**/* -diff\n"
)

vcr_cat_line <- function(txt) {
  bullet <- crayon::green(cli::symbol$tick)
  cli::cat_line(paste(bullet, txt, " "))
}

vcr_cat_info <- function(txt) {
  bullet <- cli::symbol$circle_filled
  cli::cat_line(paste(bullet, txt, " "))
}

pkg_name <- function(dir) {
  desc_path <- file.path(dir, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    stop("'DESCRIPTION' not found; are you sure it's an R package?")
  }
  data.frame(read.dcf(desc_path), stringsAsFactors = FALSE)[["Package"]]
}

suggest_vcr <- function(dir, verbose = TRUE, version = "*") {
  if (verbose)
    vcr_cat_line(sprintf(
      "Adding %s to %s field in DESCRIPTION",
      crayon::blue("vcr"),
      crayon::red("Suggests")
    ))
  desc::desc_set_dep("vcr", "Suggests", file = dir, version = version)
  invisible()
}

test_r_file_exists <- function(dir) {
  ff <- setdiff(
    list.files(file.path(dir, "tests"), full.names = TRUE, pattern = ".R|.r"),
    list.dirs(file.path(dir, "tests"), recursive = FALSE)
  )
  if (length(ff) == 0) return(FALSE)
  any(
    vapply(
      ff,
      function(z) {
        any(grepl("test_check", readLines(z)))
      },
      logical(1)
    )
  )
}

#' Setup vcr for a package
#'
#' @export
#' @param dir (character) path to package root. default's to
#' current directory
#' @param verbose (logical) print progress messages. default: `TRUE`
#' @return only messages about progress, returns invisible()
#' @details Sets a mimimum vcr version, which is usually the latest
#' (stable) version on CRAN. You can of course easily remove or change
#' the version requirement yourself after running this function.
use_vcr <- function(dir = ".", verbose = TRUE) {
  assert(dir, "character")
  stopifnot(length(dir) == 1)
  if (!dir.exists(dir)) stop("'dir' does not exist")
  check_installed(c("desc", "cli", "crayon"))

  pkg <- pkg_name(dir)
  if (verbose) vcr_cat_info(paste0("Using package: ", crayon::blue(pkg)))

  # note: assuming fixtures directory
  if (verbose)
    vcr_cat_info(paste0(
      "assuming fixtures at: ",
      crayon::blue("tests/fixtures")
    ))

  # add vcr to Suggests in DESCRIPTION file
  suggest_vcr(dir, verbose, ">= 0.6.0")

  # add tests/testthat.R if not present
  if (!dir.exists(file.path(dir, "tests/testthat"))) {
    if (verbose)
      vcr_cat_line(paste0(
        "Creating directory: ",
        file.path(dir, "tests/testthat")
      ))
    dir_create(file.path(dir, "tests/testthat"))
  }
  if (verbose) vcr_cat_info("Looking for testthat.R file or similar")
  tall <- file.path(dir, "tests/testthat.R")
  if (!test_r_file_exists(dir)) {
    if (verbose)
      vcr_cat_line(paste0(crayon::blue("tests/testthat.R:"), " added"))
    file.create(tall, showWarnings = FALSE)
    cat(sprintf(vcr_text$test_all, pkg), file = tall, append = TRUE)
  } else {
    if (verbose)
      vcr_cat_info(paste0(
        crayon::blue("tests/testthat.R (or similar):"),
        " exists"
      ))
  }

  # add helper-pkgname.R to tests/testthat/
  rel_hf <- "tests/testthat/helper-vcr.R"
  abs_hf <- file.path(dir, rel_hf)
  if (!file.exists(abs_hf)) {
    file.create(abs_hf, showWarnings = FALSE)
  }
  if (!any(grepl("vcr_configure", readLines(abs_hf)))) {
    if (verbose)
      vcr_cat_line(paste0("Adding vcr config to ", crayon::blue(rel_hf)))
    cat(vcr_text$config, file = abs_hf, append = TRUE)
  } else {
    if (verbose)
      vcr_cat_info(paste0(
        "vcr config appears to be already setup in ",
        crayon::blue(rel_hf)
      ))
  }

  # add dummy test file with example of use_cassette()
  if (verbose)
    vcr_cat_line(paste0(
      "Adding example test file ",
      crayon::blue("tests/testthat/test-vcr_example.R")
    ))
  dummyfile <- file.path(dir, "tests/testthat/test-vcr_example.R")
  cat(vcr_text$example_test, file = dummyfile)

  # add .gitattributes file
  gitattsfile <- file.path(dir, ".gitattributes")
  if (!file.exists(gitattsfile)) {
    if (verbose) vcr_cat_line(paste0(crayon::blue(".gitattributes:"), " added"))
    cat(vcr_text$gitattributes, file = gitattsfile)
  } else {
    if (verbose)
      vcr_cat_info(paste0(crayon::blue(".gitattributes:"), " exists"))
    txt <- readLines(gitattsfile)
    if (
      any(grepl("tests\\/fixtures\\/\\*\\*\\/\\* -diff", txt)) &&
        any(grepl("* text=auto", txt))
    ) {
      if (verbose)
        vcr_cat_info(
          paste0(
            crayon::blue(".gitattributes"),
            " already setup to ignore cassette diffs"
          )
        )
    } else {
      if (verbose)
        vcr_cat_info(sprintf(
          "appending lines to %s to ignore cassette diffs",
          crayon::blue(".gitattributes")
        ))
      cat(vcr_text$gitattributes, file = gitattsfile, append = TRUE)
    }
  }

  # done
  if (verbose) vcr_cat_info(vcr_text$learn_more)
  invisible()
}
