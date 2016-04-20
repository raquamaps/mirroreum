#!/usr/bin/Rscript

library(devtools)

install_github("raquamaps/raquamaps")
install_github("mskyttner/swedishbirdtrends")

install.packages("tufte")
install.packages("tuftehandout")
system("sudo apt-get install texlive-generic-recommended texlive-xetex")
devtools::install_github("rstudio/rticles")


system("sudo kpsewhich -var-value TEXMFLOCAL")


source("/tmp/eubon/SpaNiche.R")
source("/tmp/eubon/TopDown_PoO.R")
source("/tmp/eubon/TopX.R")
source("/tmp/eubon/Win_PoO.R")

