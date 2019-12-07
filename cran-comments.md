## Test environments

* local OS X install, R 3.6.1 Patched
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

* I have run R CMD check on the 25 reverse dependencies.
(Summary at <https://github.com/ropensci/vcr/blob/master/revdep/README.md>).

--------

This version adds some major features including supporting http requests that write to disk, ignoring certain requests, and turning vcr completely off during a test suite run.

Thanks very much,
Scott Chamberlain
