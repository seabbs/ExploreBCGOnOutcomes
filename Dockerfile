## Start with the tidyverse docker image
FROM rocker/tidyverse:3.5.0

MAINTAINER "Sam Abbott" contact@samabbott.co.uk

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
    
ADD . /home/rstudio/ExploreBCGOnOutcomes

WORKDIR /home/rstudio/ExploreBCGOnOutcomes

RUN mkdir -p .checkpoint/.checkpoint

Run Rscript -e 'setwd("scripts"); source("packages.R")'
