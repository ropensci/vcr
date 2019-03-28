vcr 0.2.6
=========

## NEW FEATURES

* gains function `use_vcr()` to setup `vcr` for your package. This requires 3 pkgs all in Suggests; so are not required if you don't need to use `use_vcr()` (#52) (#95) thanks @maelle for the feedback!
* `vcr` actually supports all four recording modes: `none`, `once`, `new_episodes`, and `all`. `once` is what's used by default. See `?recording` for description of the recording modes. For now [the test file test-ause_cassette_record_modes.R](https://github.com/ropensci/vcr/blob/master/tests/testthat/test-ause_cassette_record_modes.R) gives some examples and what to expect for each record mode; in the future the http testing book will have much more information in the _Record modes_ chapter <https://ropensci.github.io/http-testing-book/record-modes.html> ([commit](https://github.com/ropensci/vcr/commit/04aa5f784b18308d8f62d1b6b0be2f3e140f2a5a))

### MINOR IMPROVEMENTS

* lots of tidying for better/consistent style
* fix for a partial argument call in `as.list()`: `all` to `all.names` ([commit](https://github.com/ropensci/vcr/commit/b20a2d5ffd0f65175dee4d84aa9573f3652df1d2))

### BUG FIXES

* error thrown with `httr` due to wrong date format. the problem was in the `webmockr` package. see [ropensci/webmockr#58](https://github.com/ropensci/webmockr/issues/58) (#91) thanks @Bisaloo
* fix for `use_cassette()` when using `httr`: we weren't collecting `status_code` and storing it with the cassette (#92) thanks @Bisaloo
* fixes for `use_cassette()` for `httr`: was working fine with a single httr request, but not with 2 or more (#93) (#94) thanks @Rekyt
* in error blocks with `use_cassette()` the URL is presented from the request, and if there's a secret (API key) in the URL as a query parameter (or in any other place in the URL) then that secret is shown to the world (including if the error block happens on CI on the public web). This is fixed now; we use directives from your `filter_sensitive_data` call in `vcr_configure()` to mask secrets in error messages (#89) (#90)


vcr 0.2.2
=========

### MINOR IMPROVEMENTS

* typo fixes (#85) thanks @Rekyt
* added to docs: at least one person has reported different results using `vcr` with `devtools::check` vs. `devtools::test` (#83)
* changed suggested usage of `vcr` in test suites from `use_cassette` block wrapped in `test_that` to the other way around; leads to `testthat` pointing to the actual test line that failed rather than pointing to the start of the `use_cassette` block (#86)

### BUG FIXES

* Fix for `%||%` internal function. Was incorrectly doing logical comparison; when headers list was passed one or more of the tests in the if statement had length > 1. Dev R is testing for this (#87)


vcr 0.2.0
=========

### NEW FEATURES

* gains support for the `httr` package. `vcr` now supports `crul` and `httr`. Some of the integration for `httr` is via `webmockr`, while some of the tooling resides here in `vcr`  (#73) (#79)

### BUG FIXES

* fix handling of response bodies when not raw type (#77) (#78)


vcr 0.1.0
=========

### NEW FEATURES

* released to CRAN
