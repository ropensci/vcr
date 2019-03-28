tmpdir <- tempdir()
library(vcr)
vcr_configure(dir = tmpdir)


desc_text <- "Package: %s
Title: Does A Thing
Description: Does a thing.
Version: 0.0.1
Author: Jane Doe
Maintainer: Jane Doe <jane@doe.com>
License: MIT + file LICENSE
LazyData: true
RoxygenNote: 6.1.1
Suggests:
    testthat\n"

make_pkg <- function(dir) {
  if (length(list.files(dir)) > 1)
    stop("dir is not empty")
  dir.create(dir, recursive = TRUE)
  dir.create(file.path(dir, "man"), recursive = TRUE)
  dir.create(file.path(dir, "R"), recursive = TRUE)
  cat(sprintf(desc_text, basename(dir)), file = file.path(dir, "DESCRIPTION"))
}
