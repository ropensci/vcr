vcr (development version)
=========

### BREAKING CHANGES

* `cassettes()` are now a stack. The most important consequence of this is that `eject_cassette()` can only remove the most recently inserted cassette.
* `as.cassette()` has been removed. It's not used, and not needed anymore.
* `cassettes()` no longer has `on_disk` or `verb` arguments and now only ever lists currently active cassettes().

## NEW FEATURES

* `local_vcr_configure()` allows you to temporarily affect vcr configuration.

vcr 1.7.0
=========

### MINOR IMPROVEMENTS

* Change maintainer email address (#274)


vcr 1.6.0
=========

### NEW FEATURES

* `vcr` now supports `httr2` in addition to `httr` and `crul`. (#237) (#268)
* `vcr` now supports async http requests with `crul` (w/ `crul` v1.5 or greater). no change was required in `vcr` for this to happen. a PR was merged in `crul` to hook into `vcr`. there's no support for async in `httr` as that package does not do any async and no support in `httr2` because `req_perform_parallel` does not have a mocking hook as does `req_perform` (#246)

### BUG FIXES

* Ports in URLs (e.g., 8000) were being accidentally stripped. Fixed now (#264) (#266)

### MINOR IMPROVEMENTS

* Add link to DESCRIPTION file for packge documentation. thanks @olivroy (#265)
* Use `_PACKAGE` syntax for package level doc (#263)
* Improvements to the cassette editing vignette (#262) thanks @adamhsparks


vcr 1.2.2
=========

* change tests to use more reliable test servers

vcr 1.2.0
=========

### NEW FEATURES

* Added @dpprdan as an author; changed all ctb to aut (#258)

### MINOR IMPROVEMENTS

* `use_vcr()` now creates a test helper file called `helper-vcr.R` instead of `setup-pkgname.R`.
  We are reverting the change from version 0.6.0 and now recommend the use of `helper-*.R` again, so that the vcr setup [is loaded with `devtools::load_all()`](https://testthat.r-lib.org/reference/test_dir.html#special-files).
  That way your vcr-enabled tests also work when run interactively (#244) (#256)
* default git branch changed from master to main (#253)
* update example packages in the README (#257)
* vcr no longer requires compilation because replaced the single C++ function with a pure R equivalent

### BUG FIXES

* roll back a change from the previous CRAN version that removed use of an internal function (`body_from`) (#249) (#252)

vcr 1.1.0
=========

### MINOR IMPROVEMENTS

* request matching was sensitive to escaping special characters, that's been fixed (#240) (#247) thanks to @KevCaz
* fix broken link given in error suggestion (#239) thanks to @maelle
* using `preserve_exact_body_bytes = TRUE` now writes a base64 encoded string into a field in yaml or json on disk called `base64_string`. When `preserve_exact_body_bytes = FALSE` (the default) the response body goes into a field called `string`

### BUG FIXES

* `vcr_test_path` fix to find root package path correctly with R 4.2 on Windows (#242) (#243) thanks to @dpprdan

vcr 1.0.2
=========

### BUG FIXES

* fix to `vcr_test_path()` to find root package path correctly (#235) (#236)

vcr 1.0.0
=========

### NEW FEATURES

* `check_cassette_names()` gains `allowed_duplicates` parameter to allow duplicate cassette names; we typically advise users not to use duplicate cassette names, but there are cases where you may want to share cassettes across tests (#227)
* `vcr_configure()` gains `filter_query_parameters` parameter for filtering out query parameters so they don't show up in the recorded request on disk (#212)
* `use_vcr()`: now sets a mimimum vcr version, which is usually the latest (stable) version on CRAN. You can of course easily remove or change the version requirement yourself after running it (#214)
* `vcr_configure()` gains `warn_on_empty_cassette` parameter: Should a warning be thrown when an empty cassette is detected? Empty cassettes are cleaned up (deleted) either way (#224) thanks @llrs and @dpprdan
* `vcr_configure()` gains `quiet` parameter: suppress any messages from both vcr and webmockr (#226) (#25)
* `vcr_configure()` gains new option `filter_sensitive_data_regex`; now `filter_sensitive_data` is for fixed string matching, while `filter_sensitive_data_regex` is for regex based matching (#222) thanks @tomsing1 for reporting
* gains package import `rprojroot`

### MINOR IMPROVEMENTS

* `filter_sensitive_data` option now strips leading and trailing single and double quotes from strings before being used IN CASE a user accidentally quotes a secret - logic being that even though a secret may have a single or double quote in it, its very unlikely that it would have both a leading and trailing quote (single or double) (#221)

### Documentation

* new vignette explaining the design of the vcr package (also can be found in the HTTP Testing book) (#232) (#233)
* no user facing change - but vignettes moved into man/rmdhunks so that they can be pulled into the HTTP Testing book easily (#209) (#216)
* fix in configuration vignette to clarify a `filter_request_headers` example  (#215) thanks @maelle
* docs update (#33) (#217)

### BUG FIXES

* `filter_request_headers` was unfortunately adding a request header to the request written to disk when the header did not exist; now fixed (#213)
* bug in internal function `is_base64()`; `strsplit()` needed `useBytes=TRUE` (#219)
* `filter_sensitive_data` was not working when strings contained regex characters; fixed, and see also above new config variable for regex specific filtering  (#222) thanks @tomsing1 for reporting
* `vcr_test_path()` should now correctly set paths (#225) (#228) (#229) (#230)


vcr 0.6.0
=========

### NEW FEATURES

* We have a new vcr contributor! @maelle (#198)
* Gains a new serializer: JSON. You can use this serializer by setting globally `vcr_configure(serialize_with="json")` or per cassette `use_cassette(..., serialize_with="json")`. The JSON serializer uses `jsonlite` under the hood. Note that we by default do not write JSON to disk preserving newlines; that is the JSON is all on one line. You can use pretty printing by setting `json_pretty` in `vcr_configure()`. As part of this change, factored out new R6 class `Serializer` from which both JSON and YAML serializers inherit (#32)
* Gains two new configuration options for managing secrets: `filter_request_headers` and `filter_response_headers`. These are implemented differently than `filter_sensitive_data`. The two new filters do simple value replacement or complete removal of request or response headers, whereas `filter_sensitive_data` uses regex to replace strings anywhere in the stored request/response. See the "Configure vcr" vignette for details (#182)
* request matching: `host` and `path` now work (#177) (see also #70)
* In previous versions of vcr the `insert_cassette()`/`eject_cassette()` workflow did not work because the webmockr triggers required only worked when using `use_cassette()`. This has been fixed now so you can use `use_cassette()`, passing a code block to it, or run `insert_cassette()` then run any code, then when finished run `eject_cassette()`.  (#24) thanks @Robsteranium for the nudge, may not have fixed this without it
* improve debugging experience: new vignette "Debugging your tests that use vcr", including new function `vcr_test_path()` - which is now used in `use_vcr()` so that the correct path to tests is used when running tests both interactively and non-interactively (#192) (#193)
* Dependencies: dropped `lazyeval` from Imports; `withr` added to Suggests; minimum `webmockr` version now `v0.7.4`
* In README, point to rOpenSci code of conduct rather than file in repo
* Gains function `skip_if_vcr_off()` to use in tests to skip a test if vcr is turned off (#191) (#195)

### MINOR IMPROVEMENTS

* slight factor out of some code in YAML serializer to use elsewhere (#203) (#204)
* serializers: drop `$deserialize_string()` method as was not used - rename `$deserialize_path()` method to just `$deserialize()` (#189)
* serializers: with the new JSON serializer, documentation added to `?vcr_configure` and `?use_cassette` stating that you can have different cassettes with the same name as long as they use different serializers (and then have different file extensions). if you want to change serializers but do not want to keep the old cassette with the old serializer make sure to clean up the old file (#188)
* now using GitHub Actions - remove Travis-CI and Appveyor (#175)
* fixes for tests not being idempotent (#174) thanks @alex-gable
* clean up `UnhandledHTTPRequestError` - remove unused variable `cassette` in `$initialize()` method (always use `current_cassette()` to get the cassette being used) (#163) tip from @aaronwolen
* A change in latest webmockr release (`v0.7.4`) allowed for changes here to return an httr `response` object that more closely matches what httr returns in a real HTTP request. Before this the major problem was, assuming `x` is a httr `response` object, `x$request` was a `RequestSignature` object (from `webmockr`), whereas the class in a real httr response object is `request`  (#132)
* Re-factor of `Cassette` class greatly simplifying webmockr HTTP request stubbing (#98) (#173) big thanks to @alex-gable !
* `HTTPInteractionList` improvement: in checking for request matches against those on disk we were checking all requesets in a cassette - faster to check and stop when a match found. Using new factored out function to do this checking that stops when first match found. Many more tests added to check this behavior (#69)
* base64 encoded output in cassettes when using YAML serializer are now wrapped to approximately 80 character width (triggered when `preserve_exact_body_bytes=TRUE`) - this makes cassettes longer however. Implementing this brought in use of `cpp11` (first use of C++ in vcr). This makes base64 encoded response body recording consistent with how vcr's in other programming languages do it  (#41)
* `decode_compressed_response` option removed from `Cassette` class - wasn't being used and won't be used (#30)
* add additional examples to `VcrResponse` docs showing what the `update_content_length_header()` does (#29)
* `use_vcr()` changes: 1) now creates a test helper file called `setup-pkgname.R` instead of `helper-pkgname.R`; 2) now by default sets directory for fixtures using `dir = vcr_test_path("fixtures")` instead of `dir = "../fixtures"`. See other news item about `vcr_test_path`

### DOCUMENTATION

* better description of vcr at top of README (#198)
* delete unused docs folder in repository (docs built elsewhere) (#210)
* tell users that explicitly loading vcr is required in your test setup (#185) (#186) thanks @KevCaz
* added explanation of where and how `webmockr` is integrated in `Cassette` class - see section "Points of webmockr integration" in `?Cassette` (#176) (see also #173)
* improved getting started and protecting secrets sections in the introduction vignette (#170) (#172) thanks @DaveParr
* add to introduction vignette a section titled "how to ensure tests work in the absence of a real API key" (#137) (#194)



vcr 0.5.4
=========

### NEW FEATURES

* Error messages when tests using vcr fail are now simpler, primarily to reduce the space error messages take up. The user can toggle whether they get the new simplified error messages or the older format more verbose messages using the `verbose_errors` setting in the `vcr_configure()` function. In addition, `vcr_last_error()` gives the last full error, but that doesn't help in non-interactive mode; if in non-interactive mode, which most users will be in when running the entire test suite for a package, you can set an environment variable (`VCR_VERBOSE_ERRORS`) to toggle this setting (e.g.,
`Sys.setenv(VCR_VERBOSE_ERRORS=TRUE); devtools::test()`) (#121) (#154)

### MINOR IMPROVEMENTS

* changed `write_disk_path` handling internally to not run it through `normalizePath` before recording it to the cassette; passing the path through `normalizePath` was leading to the full path recorded in the cassette, which means in a package testing context that a test that uses a file on disk will (likely) only work on the machine the cassette was first created on. with relative paths in a package context, a test that has a file written on disk should now work in different testing contexts (locally, and various continuous integration platforms) (#135) (#166)
* added a bit of documentation about large files created when using vcr, and how to ignore them if needed within `.Rinstignore` and/or `.Rbuildignore` (#164)

vcr 0.5.0
=========

### NEW FEATURES

* new function `check_cassette_names` to use in your `helper-pkgname.R` file in your test suite; it checks for duplicated cassette names only. Any use of `insert_cassette()` (thereby, any use of `use_cassette()`) uses a revamped version of an internal fxn that checks for an improved list of potential problems in cassette names (#116) (#159)
* `use_vcr()` adds gitignore cassette diffs via the addition of a `gitattributes` file (#109)
* `vcr_configure()` overhaul: function no longer has each setting as a parameter; rather, it has an ellipsis (`...`), and internally we check parameters passed in. The documentation (`?vcr_configure`) lists the details for each available parameter. Importantly, each call to `vcr_configure()` now only changes the vcr settings for parameters passed in to the function; to reset all vcr settings, run `vcr_configure_reset()`  (#136) (#141)
* `insert_cassette()` and `use_cassette()` now inherit any vcr settings set by `vcr_configure()`; this wasn't happening consistently before. Most default parameter values in `insert_cassette/use_cassette` set to `NULL`, in which case they inherit from whatever values are set by `vcr_configure()`, but can be overriden (#151) (#153)

### MINOR IMPROVEMENTS

* define _serialize_, _cassette_, and _fixture_ in the README (#138) (#139)
* fix `filter_sensitive_data` parameter description in `vcr_configure` docs  (#129)
* move higher up in README a brief description of what this package does (#140)
* import `utils::getParseData` so its in namespace (#142)
* better cleanup of some stray test files left on disk (#148)
* `use_vcr()` no longer uses `context()` in example test file (#144)
* improved documentation of functions and environment variables for turning vcr on and off and when to use each of them - documentation mostly in the HTTP Testing book at https://books.ropensci.org/http-testing/lightswitch.html (#131)
* fix a `use_cassette` test (#133)
* Add assertions to `vcr_configure()` when parameters are set by the user to fail early (#156)

### BUG FIXES

* fix for handling of http requests that request image data AND do not write that data to disk; in addition, fix usage of `preserve_exact_body_bytes` when image data is in the response body (#128) thanks @Rekyt
* vcr now should handle request bodies correctly on POST requests (#143)
* Request matching was failing for empty bodies when "body" was one of the matchers (#157) (#161)
* fix to `sensitive_remove()` internal function used when the user sets `filter_sensitive_data` in `vcr_configure()`; when an env var is missing in the `filter_sensitive_data` list, `sensitive_remove()` was causing C stack errors in some cases (#160) thanks @zachary-foster
* fix for recording JSON-encoded bodies; vcr wasn't handling HTTP requests when the user set the body to be encoded as JSON (e.g., `encode="json"` with crul or httr) (#130)


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
* `vcr` actually supports all four recording modes: `none`, `once`, `new_episodes`, and `all`. `once` is what's used by default. See `?recording` for description of the recording modes. For now [the test file test-ause_cassette_record_modes.R](https://github.com/ropensci/vcr/blob/main/tests/testthat/test-ause_cassette_record_modes.R) gives some examples and what to expect for each record mode; in the future the http testing book will have much more information in the _Record modes_ chapter <https://books.ropensci.org/http-testing/record-modes.html> ([commit](https://github.com/ropensci/vcr/commit/04aa5f784b18308d8f62d1b6b0be2f3e140f2a5a))

### MINOR IMPROVEMENTS

* lots of tidying for better/consistent style
* fix for a partial argument call in `as.list()`: `all` to `all.names`

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
