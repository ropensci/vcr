# contents of test_addOne.R
#
#	tinytest::run_test_file("test_addOne.R")
addOne <- function(x) x + 2
expect_true(addOne(0) > 0)
hihi <- 1
expect_false(addOne(hihi) == 2)
