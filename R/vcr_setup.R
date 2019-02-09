vcr_text <- list(
  test_all = "library(\"testthat\")
test_check(\"%s\")\n",
  config = "library(\"vcr\")
invisible(vcr::vcr_configure(
  dir = \"../fixtures\"
))\n",
  example_test = "# RUN AND DELETE ME
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
    "https://ropensci.github.io/http-testing-book/"
  )
)

vcr_cat_line <- function(txt) {
  bullet <- crayon::green(cli::symbol$tick)
  cli::cat_line(paste(bullet, txt, " "))
}

pkg_name <- function(dir) {
  data.frame(read.dcf(file.path(dir, "DESCRIPTION")),
    stringsAsFactors = FALSE)[["Package"]]
}

suggest_vcr <- function(dir) {
  vcr_cat_line(sprintf("Adding %s to %s field in DESCRIPTION",
    crayon::blue("vcr"),
    crayon::red("Suggests")))
  desc::desc_set_dep("vcr", "Suggests", file = dir)
  invisible()
}

#' Setup vcr for a package
#'
#' @export
#' @param dir (character) path to package root. default's to
#' current directory
#' @return only messages about progress, returns invisible()
#' @examples \dontrun{
#' dir <- file.path(tempdir(), "foobar")
#' dir.create(dir, recursive = TRUE)
#' foo <- function() {
#'   cli <- crul::HttpClient$new("https://httpbin.org")
#'   cli$get("get")
#' }
#' package.skeleton(name = "mypkg", list = "foo", path = dir)
#' pkgpath <- file.path(dir, "mypkg")
#' list.files(pkgpath)
#' pkgpath
#'
#' # install pkg
#' devtools::install(pkgpath)
#'
#' # setup vcr
#' use_vcr(dir = pkgpath)
#'
#' # run test
#' testthat::test_file(file.path(pkgpath, "tests/testthat/test-vcr_example.R"))
#'
#' # cleanup
#' remove.packages("mypkg")
#' }
use_vcr <- function(dir = ".") {
  invisible(lapply(c("desc", "cli", "crayon"), check_for_a_pkg))

  pkg <- pkg_name(dir)
  vcr_cat_line(paste0("Using package: ", crayon::blue(pkg)))

  # add vcr to Suggests in DESCRIPTION file
  suggest_vcr(dir)

  # add tests/testthat.R if not present
  tall <- file.path(dir, sprintf("tests/testthat.R", pkg))
  if (!dir.exists(file.path(dir, "tests/testthat"))) {
    dir.create(file.path(dir, "tests/testthat"), recursive = TRUE)
  }
  if (!file.exists(tall)) {
    vcr_cat_line(paste0(crayon::blue("tests/testthat.R:" ), " added"))
    file.create(tall, showWarnings = FALSE)
    cat(sprintf(vcr_text$test_all, pkg), file = tall, append = TRUE)
  } else {
    vcr_cat_line(paste0(crayon::blue("tests/testthat.R:" ), " exists"))
  }

  # add helper-pkgname.R to tests/testthat/
  hf <- file.path(dir, sprintf("tests/testthat/helper-%s.R", pkg))
  if (!file.exists(hf)) {
    file.create(hf, showWarnings = FALSE)
  }
  if (!any(grepl("vcr_configure", readLines(hf)))) {
    vcr_cat_line(paste0("Adding vcr config to ",
      crayon::blue(sprintf("tests/testthat/helper-%s.R", pkg))))
    cat(vcr_text$config, file = hf, append = TRUE)
  } else {
    vcr_cat_line(paste0("vcr config appears to be already setup in ",
      crayon::blue(sprintf("tests/testthat/helper-%s.R", pkg))))
  }

  # add dummy test file with example of use_cassette()
  vcr_cat_line(paste0("Adding example test file ",
    crayon::blue("tests/testthat/test-vcr_example.R")))
  dummyfile <- file.path(dir, "tests/testthat/test-vcr_example.R")
  cat(vcr_text$example_test, file = dummyfile)

  # done
  vcr_cat_line(vcr_text$learn_more)
  invisible()
}
