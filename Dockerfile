FROM rocker/shiny-verse:4.5.1

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    gfortran \
    git \
    curl \
    libblas-dev \
    libfftw3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libcairo2-dev \
    libxt-dev \
    libgit2-dev \
    libgsl-dev \
    libglpk-dev \
    libgmp-dev \
    liblapack-dev \
    libmpfr-dev \
    libnlopt-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server

COPY . /srv/shiny-server

RUN R -e "options(repos = c(CRAN='https://cloud.r-project.org')); install.packages(c('BiocManager','remotes'))" && \
    R -e "options(repos = c(CRAN='https://cloud.r-project.org')); install.packages(c('devtools','shiny','ggplot2','colourpicker','showtext','ggpubr','rstatix','curl','dunn.test','dplyr','FactoMineR','factoextra','vegan','tidyverse','RColorBrewer','reshape2','pheatmap','tidyr','DT','sortable','gggenes','svglite','tidytext'), dependencies = TRUE)" && \
    R -e "BiocManager::install(c('EnhancedVolcano','DESeq2','ANCOMBC','phyloseq','DirichletMultinomial','rtracklayer'), ask = FALSE, update = FALSE)" && \
    R -e "remotes::install_github('pmartinezarbizu/pairwiseAdonis/pairwiseAdonis')"

EXPOSE 3839

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/module_app.R', host='0.0.0.0', port=3839)"]
