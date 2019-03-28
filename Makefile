RSCRIPT = Rscript --no-init-file

all: move rmd2md

move:
		cp inst/vign/vcr_vignette.md vignettes;\
		cp inst/vign/configuration.md vignettes

rmd2md:
		cd vignettes;\
		mv vcr_vignette.md vcr_vignette.Rmd;\
		mv configuration.md configuration.Rmd

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

eg:
	${RSCRIPT} -e "devtools::run_examples()"

