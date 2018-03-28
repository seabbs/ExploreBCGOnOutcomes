## Start with the tidyverse docker image
FROM rocker/tidyverse:latest

MAINTAINER "Sam Abbott" contact@samabbott.co.uk

ADD . /home/rstudio/ExploreBCGOnOutcomes

RUN Rscript -e 'setwd("/home/rstudio/ExploreBCGOnOutcomes"); install.packages("packrat"); packrat::restore()'