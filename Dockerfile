## Start with the tidyverse docker image
FROM rocker/tidyverse:3.5.0

MAINTAINER "Sam Abbott" contact@samabbott.co.uk

RUN apt-get install -y \
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

Run Rscript -e 'source("scripts/packages.R")'
