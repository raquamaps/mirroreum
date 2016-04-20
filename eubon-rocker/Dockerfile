FROM rocker/ropensci:latest

RUN apt-get update \
  && apt-get install -y --no-install-recommends -t unstable \
	texlive-generic-recommended \
    libmpfr-dev

RUN install2.r --error \
	downscale \
	getopt \
	tufte \
	tuftehandout

RUN rm -rf /tmp/*.rds
#  && install2.r --error \
# && installGithub.r \
#    mskyttner/swedishbirdtrends \
#	raquamaps/raquamaps

RUN mkdir -p /tmp/eubon
WORKDIR /tmp/eubon
ADD ./*.R /tmp/eubon/
ADD ./*.zip /tmp/eubon/
RUN unzip HieRanFor*.zip
RUN unzip UpScaling*.zip
RUN Rscript -e "setwd('/tmp/eubon/'); install.packages(c('HieRanFor', 'UpScaling'), repos = NULL)"

WORKDIR /home/rstudio

#ADD docker-entrypoint.sh /docker-entrypoint.sh
#ENTRYPOINT ["/docker-entrypoint.sh"]

WORKDIR /home/rstudio
#CMD ["/usr/bin/supervisord"]
EXPOSE 8787
