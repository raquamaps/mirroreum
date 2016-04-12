#!/usr/bin/Rscript

library(devtools)

install_github("raquamaps/raquamaps")
install_github("mskyttner/swedishbirdtrends")

source("SpaNiche.R")
source("TopDown_PoO.R")
source("TopX.R")
source("Win_PoO.R")

