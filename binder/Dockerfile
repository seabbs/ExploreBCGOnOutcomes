FROM rocker/binder:3.5.0

USER root

RUN apt-get update -y \
    && apt-get install -y \
    texlive-latex-recommended \
    texlive-fonts-extra \
    texinfo \
    libqpdf-dev \
    libxt-dev \
    libudunits2-dev \
    libgdal-dev \
    libmagick++-dev \
    && apt-get clean

COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}

## Become normal user again
USER ${NB_USER}

RUN mkdir -p .checkpoint/.checkpoint

Run Rscript -e 'setwd("scripts"); source("packages.R")'