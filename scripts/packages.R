
# packages used in this module --------------------------------------------
package_names <- c('tidyverse', 'splines', 'devtools', 'parallel', 
                   'pryr', 'cowplot', 'reshape2', 'knitr', 'lme4', 'plotly',
                   'powerMediation', 'compiler', 'pander', 'mice', 'pomp', 
                   'bookdown', 'matrixStats', 'haven', 'lattice', 'stringr',
                   'forcats', 'epiR')

pkgLoad <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE, repos='http://www.stats.bris.ac.uk/R/')
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  return(paste0('Loaded ', x, ' successfully'))
}

pck <- lapply(package_names, pkgLoad)
print(pck)

#library install via github
library(modelr)
library(purrr)
library(broom)
