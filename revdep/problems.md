# rnoaa

Version: 0.8.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > test_check("rnoaa")
      Loading required package: rnoaa
      ── 1. Failure: arc2 fails with appropriate error messages (@test-arc2.R#11)  ───
      `arc2(date = "1978-01-01")` threw an error with unexpected message.
      Expected match: "must be between 1979 and 2018"
      Actual message: "dates[1] must be between 1979 and 2019"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 94 SKIPPED: 50 FAILED: 1
      1. Failure: arc2 fails with appropriate error messages (@test-arc2.R#11) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

