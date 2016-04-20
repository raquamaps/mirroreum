nginx="/usr/share/nginx/html/"
shiny="/srv/shiny-server/"
jekyll="/home/rstudio/jekyll/page/"

all: static

static:
	cp eubon/*.html eubon/*.pdf eubon/*.docx $(nginx)
	cp eubon/*.html $(jekyll)
#	cp $(nginx)/README.html $(nginx)/index.html

clean:
	rm -rf $(shiny)/*
	rm -rf $(nginx)/*

