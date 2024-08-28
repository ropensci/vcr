lns <- readLines('README.Rmd')
vcr_start <- grep("vcr", lns)[1]
f <- basename(tempfile(fileext = ".Rmd"))
cat(lns[vcr_start:length(lns)], file=f, sep="\n")
knitr::knit(f, output = "README.md")
unlink(f)
