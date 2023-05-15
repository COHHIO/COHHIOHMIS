FROM rocker/r-ver:4.2.1

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

RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN mkdir /home/rmdata

WORKDIR /home/rmdata

COPY renv.lock renv.lock

ENV RENV_PATHS_LIBRARY renv/library

ADD clarity.looker /home/clarity.looker

COPY . /home/rmdata

RUN R -e 'options(timeout = max(5000, getOption("timeout")))'

RUN R -e "install.packages('littler', dependencies = TRUE)"

RUN R -e "renv::init();renv::restore(exclude=c('arrow','clarity.looker','HMIS','lookr')); renv::snapshot()"

RUN R -e "install.packages('pkgload', dependencies = TRUE, repos = c(CRAN = 'https://cloud.r-project.org'))"

RUN R -e "install.packages('aws.s3', dependencies = TRUE, repos = c(CRAN = 'https://cloud.r-project.org'))"

RUN install2.r --error --deps TRUE arrow

RUN R -e "remotes::install_github('COHHIO/HMIS')"
RUN R -e "remotes::install_github('COHHIO/lookr')"
RUN R -e "remotes::install_github('COHHIO/clarity.looker')"

CMD ["R"]