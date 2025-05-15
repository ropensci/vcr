# ipumsr

<details>

* Version: 0.8.2
* GitHub: https://github.com/ipums/ipumsr
* Source code: https://github.com/cran/ipumsr
* Date/Publication: 2025-02-24 21:40:02 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "ipumsr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ipums-api-micro.Rmd’ using rmarkdown
    
    Quitting from ipums-api-micro.Rmd:159-161 [unnamed-chunk-7]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `eject_cassette()`:
    ! unused argument ("micro-sample-ids")
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ...
    --- finished re-building ‘ipums.Rmd’
    
    --- re-building ‘value-labels.Rmd’ using rmarkdown
    --- finished re-building ‘value-labels.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ipums-api-micro.Rmd’ ‘ipums-api-nhgis.Rmd’ ‘ipums-api.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# qualtRics

<details>

* Version: 3.2.1
* GitHub: https://github.com/ropensci/qualtRics
* Source code: https://github.com/cran/qualtRics
* Date/Publication: 2024-08-16 16:20:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "qualtRics")` for more info

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
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
       18.                 └─vcr:::vcr_response(body = response_body$data)
       19.                   └─vcr:::stop_input_type(body, "a string, raw vector, or NULL")
       20.                     └─rlang::abort(message, ..., call = call, arg = arg)
      
      [ FAIL 10 | WARN 19 | SKIP 16 | PASS 60 ]
      Error: Test failures
      In addition: Warning message:
      In readRenviron("~/.Renviron") :
        file '/root/.Renviron' cannot be opened for reading
      Execution halted
    ```

# rATTAINS

<details>

* Version: 1.0.0
* GitHub: https://github.com/mps9506/rATTAINS
* Source code: https://github.com/cran/rATTAINS
* Date/Publication: 2023-04-25 14:00:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "rATTAINS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    
    Quitting from Introduction.Rmd:48-50 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `vcr::eject_cassette()`:
    ! unused argument ("state_summary")
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ...
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    unused argument ("state_summary")
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Rtumblr

<details>

* Version: 0.1.0
* GitHub: https://github.com/schochastics/Rtumblr
* Source code: https://github.com/cran/Rtumblr
* Date/Publication: 2023-04-05 10:23:18 UTC
* Number of recursive dependencies: 52

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
        8.           └─base::lapply(x, f)
        9.             └─vcr (local) FUN(X[[i]], ...)
       10.               └─vcr:::should_be_ignored(x$request)
       11.                 └─curl::curl_parse_url(request$uri)
      
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      In addition: Warning message:
      `check_cassette_names()` was deprecated in vcr 2.0.0. 
      Execution halted
    ```

