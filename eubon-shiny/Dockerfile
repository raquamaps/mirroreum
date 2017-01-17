FROM raquamaps/mirroreum:v0

RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev

RUN installGithub.r --deps TRUE \
	mskyttner/swedishbirdrecoveries

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cran.rstudio.com/')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

# clean out example and deploy swedishbirdrecoveries in root context
RUN rm /srv/shiny-server/index.html && rm -rf /srv/shiny-server/sample-apps
RUN ln -s /usr/local/lib/R/site-library/swedishbirdrecoveries/shiny-apps/birdrecoveries/* /srv/shiny-server
#RUN ln -s /usr/local/lib/R/site-library/swedishbirdrecoveries/shiny-apps/birdrecoveries

CMD ["/usr/bin/shiny-server.sh"]
