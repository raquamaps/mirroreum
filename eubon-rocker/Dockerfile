FROM rocker/ropensci:latest

RUN install2.r --error \
	getopt

ADD ./*.R /home/rstudio/

#ADD docker-entrypoint.sh /docker-entrypoint.sh
#ENTRYPOINT ["/docker-entrypoint.sh"]

WORKDIR /home/rstudio
EXPOSE 8787

