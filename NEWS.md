vcr 0.4.0
=========

### NEW FEATURES

* vcr now can handle requests from both `crul` and `httr` that write to disk; `crul` supports this with the `disk` parameter and `httr` through the `write_disk()` function; see the section on mocking writing to disk in the http testing book https://books.ropensci.org/http-testing/vcr-usage.html#vcr-disk; also see `?mocking-disk-writing` within `webmockr` for mocking writing to disk without using `vcr`, and the section in the http testing book https://books.ropensci.org/http-testing/webmockr-stubs.html#webmockr-disk  (#81) (#125)
* vcr gains ability to completely turn off vcr for your test suite even if you're using `vcr::use_cassette`/`vcr::insert_cassette`; this is helpful if you want to run tests both with and without vcr; workflows are supported both for setting env vars on the command line as well as working interactively within R; see `?lightswitch` for details (#37)
* ignoring requests now works, with some caveats: it only works for now with `crul` (not `httr`), and works for ignoring specifc hosts, and localhosts, but not for custom callbacks. See the vcr configuration vignette https://docs.ropensci.org/vcr/articles/configuration.html#ignoring-some-requests for discussion and examples (#127)

### MINOR IMPROVEMENTS

* documentation for R6 classes should be much better now; roxygen2 now officially supports R6 classes (#123)
* added minimal cassette name checking; no spaces allowed and no file extensions allowed; more checks may be added later (#106)

### BUG FIXES

* fix handling of http response bodies that are images; we were converting raw class bodies into character, which was causing images to error, which can't be converted to character; we now check if a body can be converted to character or not and if not, leave it as is (#112) (#119) thanks @Rekyt for the report
* simple auth with package `httr` wasn't working (`htrr::authenticate()`); we were not capturing use of `authenticate`; it's been solved now (#113)
* we were not properly capturing request bodies with package `httr` requests; that's been fixed (#122)
* httr adapter was failing on second run, reading a cached response. fixed now (#124)
* `response_summary()` fixed; this function prints a summary of the http response body; sometimes this function would fail with multibyte string error because the `gsub` call would change the encoding, then would fail on the `substring` call; we now set `useBytes = TRUE` in the `gsub` call to avoid this problem (#126)


vcr 0.3.0
=========

### NEW FEATURES

* new internal method `up_to_date_interactions` in `cassette_class` now allows filtering cassettes by user specified date (#96) (#104)
* re-recording now works - see new `use_casette()` parameters `re_record_interval` and `clean_outdated_http_interactions`; you can now set a re-record interval (in seconds) so that you can for example always re-record cassettes if you don't want cassettes to be more than X days old; depends on new internal method `up_to_date_interactions` (#104) (#105)

### MINOR IMPROVEMENTS

* fix link to HTTP Testing Book: ropensci -> ropenscilabs (#100)
* add new section to HTTP Testing Book on "vcr enabled testing" with sub-sections on check vs. test, your package on CRAN, and your package on continuous integration sites (#102)

### BUG FIXES

* fix request body matching - partly through fixes to `webmockr` package (requires v0.4 or greater); more generally, makes single type request matching (e.g., just HTTP method, or just URL) possible, it was not working before, but is now working; added examples of doing single type matching (#70) (#76) (#108)
* fixed type in `cassette_class` where typo lead to not setting headers correctly in the `webmockr::wi_th()` call (#107)


vcr 0.2.6
=========

## NEW FEATURES

* gains function `use_vcr()` to setup `vcr` for your package. This requires 3 pkgs all in Suggests; so are not required if you don't need to use `use_vcr()` (#52) (#95) thanks @maelle for the feedback!
* `vcr` actually supports all four recording modes: `none`, `once`, `new_episodes`, and `all`. `once` is what's used by default. See `?recording` for description of the recording modes. For now [the test file test-ause_cassette_record_modes.R](https://github.com/ropensci/vcr/blob/master/tests/testthat/test-ause_cassette_record_modes.R) gives some examples and what to expect for each record mode; in the future the http testing book will have much more information in the _Record modes_ chapter <https://books.ropensci.org/http-testing/record-modes.html> ([commit](https://github.com/ropensci/vcr/commit/04aa5f784b18308d8f62d1b6b0be2f3e140f2a5a))

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
