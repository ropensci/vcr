#' Using \pkg{vcr} for unit testing
#'
#' @section Using \pkg{vcr} with \pkg{testthat}:
#' \pkg{vcr} supports use with the \pkg{testthat} package. Here's the steps
#' to follow:
#'
#' First, note that \pkg{vcr} only works with a single HTTP client for
#' now: \pkg{crul}
#'
#' \itemize{
#'  \item In addition to \pkg{testthat}, add \pkg{vcr} and \pkg{webmockr}
#'  to your Suggests in your DESCRIPTION file
#'  \item Add a file (named e.g., `vcr-config.R`) to your `tests/testthat/`
#'  directory. In that file add your \pkg{vcr} configuration settings.
#'  See [vcr_configure] for help on configuration settings.
#'  \item For any given test use the following:
#'
#'  ```
#'  use_cassette("foobar", {
#'    aa <- hello::world()
#'    expect_is(aa, "SomeClass")
#'    expect_equal(length(aa), 3)
#'  })
#'  ```
#'
#'  And the tests will behave as normally.
#'
#'  The first request will make a real HTTP request. Following requests
#'  will pull from the cached responses on cassette.
#' }
#'
#' @name testing
#' @aliases unit-testing
NULL
