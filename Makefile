all: move rmd2md

move:
		cp inst/vign/vcr_vignette.md vignettes;\
		cp inst/vign/configuration.md vignettes

rmd2md:
		cd vignettes;\
		mv vcr_vignette.md vcr_vignette.Rmd;\
		mv configuration.md configuration.Rmd
