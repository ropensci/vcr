## Test environments

* local macOS, R 4.1.0
* ubuntu 16.04 (on GitHub Actions), R 4.1.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

* I have run R CMD check on the 47 reverse dependencies. Summary at https://github.com/ropensci/vcr/actions?query=workflow%3Arevdep Problems were found in checks, but were not related to vcr.

--------

This version fixes an issue introduced in v1 released a few weeks ago on CRAN that caused problems in 6 packages that depend on vcr.

Thanks very much,
Scott Chamberlain
