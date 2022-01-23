# thank you: https://www.kloppenborg.ca/2021/06/long-running-vignettes/

old_wd <- getwd()

setwd("vignettes/")
knitr::knit("tutorial.Rmd.original", output = "tutorial.Rmd")
knitr::purl("tutorial.Rmd.original", output = "tutorial.R")

setwd(old_wd)
