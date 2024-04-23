FROM rocker/r-ver:4.3.2

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

# Install remotes package for package installation from GitHub
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Install renv for dependency management
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Create the /home/clarity.looker directory and copy the clarity.looker contents to it
RUN mkdir -p /home/clarity.looker
ADD clarity.looker/ /home/clarity.looker

# Create the /home/rmdata directory and copy renv.lock and all other files to it
RUN mkdir /home/rmdata
COPY renv.lock /home/rmdata/
COPY . /home/rmdata/

# Set the working directory to /home/rmdata
WORKDIR /home/rmdata

ENV RENV_PATHS_LIBRARY renv/library

RUN R -e 'options(timeout = max(5000, getOption("timeout")))'

RUN R -e "renv::init();renv::restore(exclude=c('arrow','clarity.looker','HMIS','lookr')); renv::snapshot()"

RUN R -e "install.packages('pkgload', dependencies = TRUE, repos = c(CRAN = 'https://cloud.r-project.org'))"

RUN R -e "install.packages('aws.s3', dependencies = TRUE, repos = c(CRAN = 'https://cloud.r-project.org'))"

RUN R -e "install.packages('arrow', dependencies = TRUE, repos = c(CRAN = 'https://cloud.r-project.org'), version = '14.0.2.1')"

RUN R -e "remotes::install_github('COHHIO/HMIS')"
RUN R -e "remotes::install_github('COHHIO/lookr')"
RUN R -e "remotes::install_github('COHHIO/clarity.looker')"
RUN R -e "remotes::install_github('COHHIO/RmData')"

CMD ["R"]