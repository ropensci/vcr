vcr_text <- list(
  test_all = "library(\"testthat\")
test_check(\"%s\")\n",
  config = "library(\"vcr\")
invisible(vcr::vcr_configure(
  dir = \"../fixtures\"
))\n",
  example_test = "# EXAMPLE VCR USAGE: RUN AND DELETE ME
content(\"vcr text example\")

foo <- function() crul::ok('https://httpbin.org/get')

test_that(\"foo works\", {
  vcr::use_cassette(\"testing\", {
    x <- foo()
  })
  expect_true(x)
})\n",
  learn_more = paste0(
    "To learn more about `vcr`, check out the HTTP testing book:\n   ",
    "https://books.ropensci.org/http-testing/"
  )
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
  data.frame(read.dcf(desc_path),
    stringsAsFactors = FALSE)[["Package"]]
}

suggest_vcr <- function(dir, verbose = TRUE) {
  if (verbose) vcr_cat_line(sprintf("Adding %s to %s field in DESCRIPTION",
    crayon::blue("vcr"),
    crayon::red("Suggests")))
  desc::desc_set_dep("vcr", "Suggests", file = dir)
  invisible()
}

test_r_file_exists <- function(dir) {
  ff <- list.files(file.path(dir, "tests"), pattern = ".R|.r",
    full.names = TRUE)
  if (length(ff) == 0) return(FALSE)
  any(
    vapply(ff, function(z) {
      any(grepl("test_check", readLines(z)))
    }, logical(1))
  )
}

#' Setup vcr for a package
#'
#' @export
#' @param dir (character) path to package root. default's to
#' current directory
#' @param verbose (logical) print progress messages. default: `TRUE`
#' @return only messages about progress, returns invisible()
use_vcr <- function(dir = ".", verbose = TRUE) {
  assert(dir, "character")
  stopifnot(length(dir) == 1)
  if (!dir.exists(dir)) stop("'dir' does not exist")
  invisible(lapply(c("desc", "cli", "crayon"), check_for_a_pkg))

  pkg <- pkg_name(dir)
  if (verbose) vcr_cat_info(paste0("Using package: ", crayon::blue(pkg)))

  # add vcr to Suggests in DESCRIPTION file
  suggest_vcr(dir, verbose)

  # add tests/testthat.R if not present
  if (!dir.exists(file.path(dir, "tests/testthat"))) {
    if (verbose) vcr_cat_line(paste0("Creating directory: ",
      file.path(dir, "tests/testthat")))
    dir.create(file.path(dir, "tests/testthat"), recursive = TRUE)
  }
  if (verbose) vcr_cat_info("Looking for testthat.R file or similar")
  tall <- file.path(dir, sprintf("tests/testthat.R", pkg))
  if (!test_r_file_exists(dir)) {
    if (verbose) 
      vcr_cat_line(paste0(crayon::blue("tests/testthat.R:" ), " added"))
    file.create(tall, showWarnings = FALSE)
    cat(sprintf(vcr_text$test_all, pkg), file = tall, append = TRUE)
  } else {
    if (verbose) 
      vcr_cat_info(paste0(crayon::blue("tests/testthat.R:" ), " exists"))
  }

  # add helper-pkgname.R to tests/testthat/
  hf <- file.path(dir, sprintf("tests/testthat/helper-%s.R", pkg))
  if (!file.exists(hf)) {
    file.create(hf, showWarnings = FALSE)
  }
  if (!any(grepl("vcr_configure", readLines(hf)))) {
    if (verbose) vcr_cat_line(paste0("Adding vcr config to ",
      crayon::blue(sprintf("tests/testthat/helper-%s.R", pkg))))
    cat(vcr_text$config, file = hf, append = TRUE)
  } else {
    if (verbose) vcr_cat_info(paste0("vcr config appears to be already setup in ",
      crayon::blue(sprintf("tests/testthat/helper-%s.R", pkg))))
  }

  # add dummy test file with example of use_cassette()
  if (verbose) vcr_cat_line(paste0("Adding example test file ",
    crayon::blue("tests/testthat/test-vcr_example.R")))
  dummyfile <- file.path(dir, "tests/testthat/test-vcr_example.R")
  cat(vcr_text$example_test, file = dummyfile)

  # done
  if (verbose) vcr_cat_info(vcr_text$learn_more)
  invisible()
}
