FROM rocker/r-ver:4.2.1

ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=daily
ENV DEFAULT_USER=rstudio
ENV PANDOC_VERSION=default
ENV PATH=/usr/lib/rstudio-server/bin:$PATH

RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh

RUN apt-get update && apt-get install -y \
	cmake \
	git \
	libcurl4-openssl-dev \
	libfontconfig1-dev \
	libfreetype6-dev \
	libfribidi-dev \
	libgit2-dev \
	libharfbuzz-dev \
	libicu-dev \
	libjpeg-dev \
	libpng-dev \
	libssh2-1-dev \
	libssl-dev \
	libtiff5-dev \
	libxml2-dev \
	make \
	pandoc \
	zlib1g-dev \

	&& rm -rf /var/lib/apt/lists/*


# clone the repository containing the script
# RUN git clone https://github.com/COHHIO/RmData.git

# renv and R packages
ENV RENV_VERSION 0.17.3
ENV RENV_PATHS_LIBRARY renv/library

RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN mkdir /home/rmdata

COPY renv.lock /home/rmdata/renv.lock

ADD clarity.looker /home/rmdata/clarity.looker

COPY . /home/rmdata

RUN R -e 'options(timeout = max(800, getOption("timeout")))'

RUN R -e "install.packages('arrow', repos = c(CRAN = 'https://cloud.r-project.org'))"

RUN R -e 'options(timeout = max(800, getOption("timeout")))'

RUN R -e "setwd('/home/rmdata'); renv::init();renv::restore(exclude=c('arrow','clarity.looker','HMIS','lookr')); renv::snapshot()"

RUN R -e "install.packages('pkgload', repos = c(CRAN = 'https://cloud.r-project.org'))"

RUN R -e "install.packages('aws.s3', repos = c(CRAN = 'https://cloud.r-project.org'))"

EXPOSE 8787

CMD ["/init"]
