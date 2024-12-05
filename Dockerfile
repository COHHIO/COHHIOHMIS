# Specify platform and base image
FROM --platform=linux/amd64 rocker/tidyverse:latest

# Labels for maintainability
LABEL maintainer="Trevin Flickinger trevinflickinger@cohhio.org>"
LABEL description="R development environment with tidyverse and custom packages"

# Environment variables
ENV RENV_PATHS_LIBRARY=renv/library
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies in a single RUN command to reduce layers
RUN apt-get update && apt-get install -y --no-install-recommends \
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
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Set R options
RUN echo "options(timeout = max(5000, getOption('timeout')))" >> /usr/local/lib/R/etc/Rprofile.site

# Install R packages in a single layer
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'), dependencies = TRUE)"
RUN R -e "install.packages(c('renv', 'pkgload', 'aws.s3'), \
    repos = c(CRAN = 'https://cloud.r-project.org'), \
    dependencies = TRUE)"

# Create necessary directories
RUN mkdir -p /home/clarity.looker /home/rmdata

# Copy project files
COPY renv.lock /home/rmdata/
COPY clarity.looker/ /home/clarity.looker/
COPY . /home/rmdata/

# Set working directory
WORKDIR /home/rmdata

# Initialize and restore renv environment
RUN R -e "renv::init(); \
    renv::restore(exclude=c('arrow','clarity.looker','HMIS','lookr')); \
    renv::snapshot()"

# Install specific version of arrow and GitHub packages
RUN R -e "install.packages('arrow', \
    repos = c(CRAN = 'https://cloud.r-project.org'), \
    version = '14.0.2.1', \
    dependencies = TRUE)" && \
    R -e "remotes::install_github(c( \
        'COHHIO/HMIS', \
        'COHHIO/lookr', \
        'COHHIO/clarity.looker', \
        'COHHIO/RmData' \
    ))"

# Set default command
CMD ["R"]