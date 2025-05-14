# use_vcr is deprecated

    Code
      use_vcr(dir, verbose = FALSE)
    Condition
      Warning:
      `use_vcr()` was deprecated in vcr 2.0.0.

# use_vcr works

    Code
      use_vcr(dir)
    Output
      (*) Using package: {dir}  
      (*) assuming fixtures at: tests/fixtures  
      v Adding vcr to Suggests field in DESCRIPTION  
      (*) Looking for testthat.R file or similar  
      v tests/testthat.R: added  
      v Adding vcr config to tests/testthat/helper-vcr.R  
      v Adding example test file tests/testthat/test-vcr_example.R  
      v .gitattributes: added  
      (*) Learn more about `vcr`: https://books.ropensci.org/http-testing  

# use_vcr fails well

    Code
      use_vcr(5)
    Condition
      Error in `use_vcr()`:
      ! `dir` must be a single string, not the number 5.

