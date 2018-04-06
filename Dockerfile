## Start with the tidyverse docker image
FROM rocker/tidyverse:3.4.4

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

RUN Rscript -e 'setwd("/home/rstudio/ExploreBCGOnOutcomes"); install.packages("packrat"); packrat::restore()'
