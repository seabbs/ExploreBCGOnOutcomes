# packages used in this module --------------------------------------------
package_names <- c('tidyverse', 'splines', 'devtools', 'parallel', 
                   'pryr', 'cowplot', 'reshape2', 'knitr', 'lme4', 'plotly',
                   'powerMediation', 'compiler', 'pander', 'mice', 'pomp', 
                   'bookdown', 'matrixStats', 'haven', 'lattice', 'stringr',
                   'forcats', 'epiR', 'rmarkdown')

pkgLoad <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  return(paste0('Loaded ', x, ' successfully'))
}

## Set up checkpoint versioning
pkgLoad("checkpoint")
if (!dir.exists("./.checkpoint")) { dir.create("./.checkpoint")}
checkpoint("2018-06-19", scanForPackages = TRUE, checkpointLocation = "./.checkpoint")

## Install/Load all other packages
pck <- lapply(package_names, pkgLoad)
print(pck)

