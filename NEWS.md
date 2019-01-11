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
