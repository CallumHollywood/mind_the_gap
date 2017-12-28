
setup <- function(){
  packs <- c("tidyverse", "dplyr", "gapminder", "reshape2", "stringr" )
  lapply(packs, require, character.only = T)
  options(scipen = 999)
  plotsave <- "output/graphs/"
  assign("plotsave", plotsave, envir = .GlobalEnv)
}

setup()
rm("setup")


















