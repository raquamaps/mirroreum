library(knitr)
library(rmarkdown)
library(tufte)
library(tufterhandout)
library(rticles)

message("Publishing results")
Sys.setenv(PDFLATEX = "xelatex")

setwd("eubon")

render("hrf.Rmd", 
  output_format = "md_document", 
  output_file = "hrf.md", 
  encoding = "utf-8")

render("hrf.Rmd", 
  output_format = "tufte::tufte_html", 
  output_file = "hrf.html", 
  encoding = "utf-8")

render("hrf.Rmd", 
  output_format = "word_document", 
  output_file = "hrf.docx", 
  encoding = "utf-8")

render("hrf.Rmd", 
  output_format = "tufte::tufte_book", 
  output_file = "hrf.pdf", 
  encoding = "utf-8")

setwd("~")

system("sudo make static")
