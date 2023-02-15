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
	libtiff-dev \
	libxml2-dev \
	make \
	pandoc \
	zlib1g-dev \

	&& rm -rf /var/lib/apt/lists/*


# clone the repository containing the script
# RUN git clone https://github.com/COHHIO/RmData.git



WORKDIR /main
# renv and R packages
ENV RENV_VERSION 0.16.0
ENV RENV_PATHS_LIBRARY renv/library
RUN echo "options(renv.consent = TRUE)" >> .Rprofile
COPY renv.lock renv.lock
ADD renv/ /main/renv
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e 'Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")'
RUN R -e 'options(renv.config.connect.timeout = 300)'
RUN R -e 'options(timeout=300)'
RUN R -e 'renv:::renv_available_packages_entry("BH")'
# RUN R -e "renv::restore(confirm = FALSE)"
# RUN R -e "renv::snapshot(confirm = FALSE)"

# COPY . ./