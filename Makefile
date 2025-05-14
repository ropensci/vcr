PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build --no-build-vignettes .

doc:
	${RSCRIPT} -e "devtools::document()"

eg:
	${RSCRIPT} -e "devtools::run_examples(run_dontrun=TRUE)"

test:
	${RSCRIPT} -e "devtools::test()"

check:
	_R_CHECK_SYSTEM_CLOCK_=0 NOT_CRAN=true _R_CHECK_CRAN_INCOMING_=FALSE \
		${RSCRIPT} -e "rcmdcheck::rcmdcheck()"

check_windows:
	${RSCRIPT} -e "devtools::check_win_devel(); devtools::check_win_release()"

readme:
	${RSCRIPT} -e "source('make_readme.R')"
