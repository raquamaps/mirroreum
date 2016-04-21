ME=$(USER)
all: build init up

clean: stop rm

init:
	mkdir -p src shr/dynamic shr/static
	
build:
	docker-compose build

portal:
	docker-compose stop jekyll
	docker-compose run jekyll bundle exec jekyll clean
	docker-compose up -d jekyll 
	docker-compose restart portal

up:
	docker-compose up -d
	@echo "Running locally? Put {wrangler,static,dynamic,portal}.mirroreum.eu in your /etc/hosts!"

stop:
	docker-compose stop

rm:
	docker-compose rm -vf
