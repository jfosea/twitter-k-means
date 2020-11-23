#' Loads all the needed libraries for all R scripts
load_libraries <- function(){
  library(rstudioapi)
  library(factoextra)
  library(tidyverse)
  library(twitteR)
  library(tidytext)
}

#' Sets up working directory to source file path
setup_directory <- function() {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
