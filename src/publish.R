library(knitr)
library(rmarkdown)
library(tufte)
library(tufterhandout)
library(rticles)

message("Publishing results")
Sys.setenv(PDFLATEX = "xelatex")

setwd("eubon/hrf")

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

setwd("../aquamaps")

render("aquamaps.Rmd", 
  output_format = "tufte::tufte_html", 
  output_file = "aquamaps.html", 
  encoding = "utf-8")

render("aquamaps.Rmd", 
   output_format = "tufte::tufte_book", 
   output_file = "aquamaps.pdf", 
   encoding = "utf-8")

render("aquamaps.Rmd", 
  output_format = "word_document", 
  output_file = "aquamaps.docx", 
  encoding = "utf-8")

setwd("../mirroreum")

render("mirroreum.Rmd", 
       output_format = "tufte::tufte_html", 
       output_file = "mirroreum.html", 
       encoding = "utf-8")

render("mirroreum.Rmd", 
       output_format = "tufte::tufte_book", 
       output_file = "mirroreum.pdf", 
       encoding = "utf-8")

render("mirroreum.Rmd", 
       output_format = "word_document", 
       output_file = "mirroreum.docx", 
       encoding = "utf-8")

setwd("~")


system("sudo make static")
