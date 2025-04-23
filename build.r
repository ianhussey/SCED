
library(roxygen2)
#setwd("~/git/")
#devtools::create("SCED")
setwd("~/git/SCED")

devtools::document()

#devtools::build_vignettes()
devtools::check(vignettes = FALSE)
#devtools::check()


#devtools::install()
# or from github, after push
devtools::install_github("ianhussey/SCED")

library(SCED)

?SCED

detach("package:SCED", unload=TRUE)
