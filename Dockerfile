FROM rhub/r-minimal
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
	libtiff-dev \
	libxml2-dev \
	make \
	pandoc \
	zlib1g-dev \

	&& rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/# clone the repository containing the script
# RUN git clone https://github.com/COHHIO/RmData.git
RUN install2.r -e remotes renv 

COPY renv.lock renv.lock

RUN R -e 'Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")'

RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf
RUN R -e 'renv::update()'
RUN R -e 'renv::restore()'

COPY . ./