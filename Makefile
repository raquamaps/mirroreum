ME=$(USER)
all: build init up

clean: stop rm
	sudo chown -R $(ME):$(ME) nginx-conf nginx-html nginx-certs nginx-logs

init:
	mkdir -p src shr/dynamic shr/static
	
build:
#	docker-compose build --no-cache

up:
	docker-compose up -d
	echo "Running locally? Put {wrangler,static,dynamic}.local in your /etc/hosts!"
	sleep 5
	wget --retry-connrefused --tries=5 --waitretry=6 "http://wrangler.local"

	echo "Opening app!"
	firefox http://wrangler.dina-web.net/

stop:
	docker-compose stop

rm:
	docker-compose rm -vf
