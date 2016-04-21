nginx="/usr/share/nginx/html/"
shiny="/srv/shiny-server/"
jekyll="/home/rstudio/jekyll/page/"

all: static

static:

	cp eubon/hrf/*.html eubon/hrf/*.pdf eubon/hrf/*.docx $(nginx)
	cp eubon/hrf/*.html $(jekyll)

	cp eubon/aquamaps/*.html eubon/aquamaps/*.pdf eubon/aquamaps/*.docx $(nginx)
	cp eubon/aquamaps/*.html $(jekyll)
	
#	cp $(nginx)/README.html $(nginx)/index.html

clean:
	rm -rf $(shiny)/*
	rm -rf $(nginx)/*

