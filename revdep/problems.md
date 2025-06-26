# deepdep

<details>

* Version: 0.4.3
* GitHub: https://github.com/DominikRafacz/deepdep
* Source code: https://github.com/cran/deepdep
* Date/Publication: 2024-03-12 21:10:07 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "deepdep")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(deepdep)
      > 
      > test_check("deepdep")
      Error in `FUN()`:
      ! In path: "/tmp/workdir/deepdep/new/deepdep.Rcheck/tests/testthat/setup-vcr.R"
    ...
        8. │             └─base::lapply(...)
        9. │               └─testthat (local) FUN(X[[i]], ...)
       10. │                 └─testthat::source_file(path, env = env, chdir = chdir, wrap = wrap)
       11. │                   ├─base::withCallingHandlers(...)
       12. │                   └─base::eval(exprs, env)
       13. │                     └─base::eval(exprs, env)
       14. └─base::.handleSimpleError(...) at tests/testthat/setup-vcr.R:3:3
       15.   └─testthat (local) h(simpleError(msg, call))
       16.     └─rlang::abort(...)
      Execution halted
    ```

# Rtumblr

<details>

* Version: 0.1.0
* GitHub: https://github.com/schochastics/Rtumblr
* Source code: https://github.com/cran/Rtumblr
* Date/Publication: 2023-04-05 10:23:18 UTC
* Number of recursive dependencies: 51

Run `revdepcheck::cloud_details(, "Rtumblr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       16.                 └─vcr:::decode_uri(request$uri)
       17.                   └─vcr:::encode_uri(uri, filter, flip = TRUE)
       18.                     ├─base::as.list(curl::curl_parse_url(uri)$params)
       19.                     └─curl::curl_parse_url(uri)
      
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      In addition: Warning message:
      `check_cassette_names()` was deprecated in vcr 2.0.0. 
      Execution halted
    ```

# webmockr

<details>

* Version: 2.0.0
* GitHub: https://github.com/ropensci/webmockr
* Source code: https://github.com/cran/webmockr
* Date/Publication: 2025-02-11 20:30:02 UTC
* Number of recursive dependencies: 44

Run `revdepcheck::cloud_details(, "webmockr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library("testthat")
      > test_check("webmockr")
      Loading required package: webmockr
      [ FAIL 1 | WARN 0 | SKIP 45 | PASS 838 ]
      
      ══ Skipped tests (45) ══════════════════════════════════════════════════════════
      • On CRAN (44): 'test-CrulAdapter.R:6:3', 'test-CrulAdapter.R:24:3',
    ...
        9.       └─private$request_handler(req)$handle()
       10.         └─vcr:::request_summary(request_matchers = matchers)
       11.           └─vcr:::check_character(request_matchers)
       12.             └─vcr:::stop_input_type(...)
       13.               └─rlang::abort(message, ..., call = call, arg = arg)
      
      [ FAIL 1 | WARN 0 | SKIP 45 | PASS 838 ]
      Error: Test failures
      Execution halted
      Ran 43/43 deferred expressions
    ```

