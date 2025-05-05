
# vcr

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![cran
checks](https://badges.cranchecks.info/worst/vcr.svg)](https://CRAN.R-project.org/package=vcr)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-check](https://github.com/ropensci/vcr/workflows/R-check/badge.svg)](https://github.com/ropensci/vcr/actions/)
[![codecov](https://codecov.io/gh/ropensci/vcr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/vcr)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/vcr)](https://github.com/r-hub/cranlogs.app)
[![cran
version](https://www.r-pkg.org/badges/version/vcr)](https://cran.r-project.org/package=vcr)

{vcr} records and replays HTTP requests so you can test your API package
with speed and confidence. It makes your tests independent of your
internet connection (so they work on CRAN!) and because your tests get
much much faster, you can write even more, increasing the coverage of
your package. {vcr} works with {crul}, {httr} and {httr2}.

{vcr} draws inspiration from Ruby’s [vcr](https://github.com/vcr/vcr).

## Installation

``` r
# Install the latest version from CRAN
install.packages("vcr")

# Or from R-universe
install.packages(
  "vcr",
  repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org")
)

# Or the get development version from GitHub:
# install.packages("pak")
pak::pak("ropensci/vcr")
```

## Usage

Using vcr in a test is straightforward: just call
`vcr::local_cassette()`. The first time your test is run, vcr will
automatically record every HTTP request, saving the request and reponse
in `tests/testthat/_vcr`. After that, it will replay those recorded
requests, meaning that your test no longer needs an active connection.

``` r
test_that("can retrieve current version", {
  vcr::local_cassette("rl_version")
  expect_equal(rredlist::rl_version(), "2025-1")
})
```

The first argument to `local_cassette()` is the cassette name: it’s used
to name the cassette file so needs to be unique across tests. In this
case, running the above test will generate
`tests/testthat/_vcr/rl_version.yaml` which looks something like this:

``` yaml
http_interactions:
- request:
    method: GET
    uri: https://api.iucnredlist.org/api/v4/information/red_list_version
    body: ~
    headers:
      Authorization: tVirteB2iZjthFGoZCQYfCCFXinYjk8nD7Ww
      User-Agent: r-curl/6.2.2 crul/1.5.0 rOpenSci(rredlist/1.0.0)
  response:
    status: 200
    headers:
      cache-control: max-age=0, private, must-revalidate
      content-type: application/json
      etag: W/"1694e95e54c5590a355e5922b47c7cd9"
      date: Sun, 27 Apr 2025 21:48:05 GMT
    body:
      string: '{"red_list_version":"2025-1"}'
      file: no
  recorded_at: 2025-04-27 21:48:05 GMT
recorded_with: vcr/1.7.0.91, webmockr/2.0.0
```

If you look carefully at this file, you’ll spot a big problem! It
includes the `Authorization` header meaning that we’ve accidentally
embedded a copy of our credentials in the package. To avoid this
problem, make sure to read `vignette("secrets")` before continuing.

## Learn more

Start with `vignette("vcr")` to learn more about how {vcr} works,
especially how requests are matched to the recordeded cassette. You
might also enjoy the [HTTP
testing](https://books.ropensci.org/http-testing/) book for a lot more
details about {vcr}, {webmockr}, {curl} and more.

## Meta

- Please [report any issues or
  bugs](https://github.com/ropensci/vcr/issues)
- License: MIT
- Get citation information for `vcr` in R doing
  `citation(package = 'vcr')`
- Please note that this package is released with a [Contributor Code of
  Conduct](https://ropensci.org/code-of-conduct/). By contributing to
  this project, you agree to abide by its terms.
