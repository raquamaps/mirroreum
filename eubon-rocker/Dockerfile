FROM rocker/ropensci:latest

RUN apt-get update && apt-get -y upgrade && apt-get install -y \
	texlive-generic-recommended texlive-xetex \
	libmpfr-dev \
	cimg-dev \
	libhdf4-dev hdf4-tools \
	libnetcdf-dev netcdf-bin \
	proj-bin libproj-dev gdal-bin

# for knitting pdfs etc with Tufte styles
RUN kpsewhich -var-value TEXMFLOCAL

# workaround to get rgdal to work on debian
# side-effect from bleeding edge hadleyverse?
RUN apt-get install -y --allow-downgrades \
	libgdal20=2.1.1+dfsg-5 \
	libmariadbclient18=10.0.27-2 \
	libmariadbclient-dev \
	libmariadbclient-dev-compat default-libmysqlclient-dev \
	libgdal-dev

# explicitly install R pkg deps for HieRanFor and UpScaling
# since the --deps TRUE option doesn't seem to work below (?)
RUN install2.r --error \
	sparklyr \
	downscale \
	getopt \
	tufte \
	tufterhandout \
	ggmap \
	rticles \
	tint \
	cartogram \
	randomForest \
	pdfCluster \
	caret \
	treemap \
	AICcmodavg \
	SDMTools

# add semi-packaged eubon R packages
RUN mkdir -p /tmp/eubon
WORKDIR /tmp/eubon
ADD ./*.R /tmp/eubon/
ADD ./*.zip /tmp/eubon/
RUN unzip HieRanFor*.zip
RUN unzip UpScaling*.zip

# can RForge be used instead?
RUN cd /tmp/eubon && install2.r --deps TRUE --repos NULL --error \
	HieRanFor \
	UpScaling

# add packages from GitHub
RUN installGithub.r --deps TRUE \
	raquamaps/raquamaps \
	mskyttner/swedishbirdtrends \
	mskyttner/swedishbirdrecoveries \
	AtlasOfLivingAustralia/ALA4R 

# add Ecology packages

RUN install2.r --error \
	speciesgeocodeR

RUN installGithub.r --deps TRUE \
	azizka/sampbias


# clean up
RUN rm -rf /tmp/*.rds && \
	apt-get autoclean

WORKDIR /home/rstudio
EXPOSE 8787
