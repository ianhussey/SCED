# # based on guide written by https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

# # 1 dependencies
# install.packages("devtools")
# library("devtools")
# devtools::install_github("klutometis/NLP")
library(roxygen2)

# # 2 working directory
setwd("~/git/")

# # 3 create minimum folders
# devtools::create("NLP")

# # 4 write functions and documentation using roxygen formatting
# # Use below template and put these functions into the R folder.
#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}

# # 5 use roxygen to write the docs for the package

library(devtools)
setwd("~/git/SCED")
roxygen2::roxygenise()

# # 6 install the package

library(devtools)
setwd("~/git/")
install("SCED")

# # 7 place in a GitHub repo

# # 8 installation for users

# # install packages (first time only)
#install.packages("devtools")
#library(devtools)
#install_github("ianhussey/NLP")
# # load packages
library(devtools)
library(SCED)
