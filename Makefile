ME = $(USER)
PWD = $(shell pwd)

all: build init up

clean: stop rm

init:
	mkdir -p src shr/dynamic shr/static
	
build:
	docker-compose build

build-webide:

	@echo "Building docker image with Web IDE + packages..."
	@docker build -t raquamaps/mirroreum:v0 eubon-rocker

build-shiny:

	@echo ""
	@docker build -t raquamaps/shiny:v0 eubon-shiny

up-shiny:
	@echo ""
	@docker run --rm --name eubonshiny \
		-p 3838:3838 -p 1410:1410 \
		-v $(PWD)/src:/home/rstudio \
		raquamaps/shiny:v0

webide:
	@echo "Running only the Web IDE for RStudio..."

	@docker run --name mirroreum \
		-e VIRTUAL_HOST=wrangler.mirrroreum.eu \
		-e VIRTUAL_PORT=8787 \
		-e USER=rstudio -e PASSWORD=rstudio -e ROOT=TRUE \
		-p 8787:8787 \
		-v $(PWD)/src:/home/rstudio \
		raquamaps/mirroreum:v0

	@xdg-open http://localhost:8787

	@echo "You can clean up now with:"
	@echo "docker rm mirroreum"

portal:
	docker-compose restart jekyll
	docker-compose restart portal

up:
	docker-compose up -d
	@echo "Running locally? Put {wrangler,static,dynamic,portal}.mirroreum.eu in your /etc/hosts!"

stop:
	docker-compose stop

rm:
	docker-compose rm -vf
