#' Setup working directory to current path directory
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#' Install packages if packages are not yet installed
library(utils) #needed for the source to load installed.packages()
options(repos=c("https://cran.rstudio.com", getOption("repos") ) )
# designate packages to install/load
all_pkgs <- c("factoextra","tidyverse","twitteR", "tidytext", "hash", "openssl", "httpuv")
# find packages that need to be installed
already_installed <- rownames(installed.packages())
to_install <- setdiff(all_pkgs, already_installed)
if (length(to_install) > 0) {
  install.packages(to_install, dependencies=TRUE)
}


#' Loads all corresponding packages
sapply(all_pkgs, library, character.only=TRUE, logical.return=TRUE)
