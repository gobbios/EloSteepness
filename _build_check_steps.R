# this is a set of instructions to check whether the package conforms to CRAN rules
# these steps here are taken from https://github.com/paul-buerkner/brms/issues/158

devtools::clean_vignettes()
devtools::document()
devtools::clean_dll()



withr::with_envvar(c("NOT_CRAN" = "true"),
                   devtools::build_vignettes()
)

# may have to remove '/doc' from gitignore at this point

# size reductions of PDF vignettes
tools::compactPDF("doc/tutorial.pdf", gs_quality = "ebook")


withr::with_envvar(c("NOT_CRAN" = "true"),
                   devtools::build(args = c('--compact-vignettes=both'))
)


withr::with_envvar(c("NOT_CRAN" = "true"),
                   devtools::check_built("../EloSteepness_0.4.6.tar.gz", args = "--as-cran")
)

# then upload .tar.gz to wincheck
# https://win-builder.r-project.org/upload.aspx

