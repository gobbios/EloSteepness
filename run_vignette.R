# thank you: https://www.kloppenborg.ca/2021/06/long-running-vignettes/
# which seems to work for html, but not for pdf...
# old_wd <- getwd()
# 
# setwd("vignettes/")
# knitr::knit("tutorial.Rmd.original", output = "tutorial.Rmd")
# knitr::purl("tutorial.Rmd.original", output = "tutorial.R")
# 
# setwd(old_wd)



# my attempt, even more hackish...
old_wd <- getwd()

setwd("vignettes/")
file.copy(from = "tutorial.Rmd.original", to = "tutorial.Rmd")
rmarkdown::render(input = "tutorial.Rmd" ) # , output_file ="tutorial.pdf", output_format = "pdf_document",
file.rename(from = "tutorial.pdf", to = "pdf_files/tutorial.pdf")
file.remove("tutorial.Rmd")
file.remove(list.files("tutorial_files", recursive = TRUE, full.names = TRUE))

setwd(old_wd)
