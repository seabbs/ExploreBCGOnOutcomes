#Remove spurious data
file.remove(paste0('../data/', folder, '/exposure_outcome_defs.rda'))

# Define Analysis Variables -----------------------------------------------
stew(file = paste0('../data/', folder, '/exposure_outcome_defs.rda'),{
  #Define outcomes to analysis - all outcomes selected are binary
  outcomes <- c('Mortality', 'TB_mortality', 'prevdiag','pulmextrapulm', 'sputsmear')
  
  #Define Variables to analysis- BCG status is binary and Years since BCG is continous
  variables <- c('bcgvacc', 'yrsinceBCG', 'ageatvac')
  
  #Define Covariates
  covariates <- c('age', 'ethgrp', 'sex', 'natquintile', 'year')
  
  #Define variables to fit to splines
  splines <- ('age')
  
  #Define outcome in words
  outcomes_words <- c('All-cause mortality', 'Death due to TB (in those who died*)', 'Recurrent TB', 'Pulmonary TB', 'Sputum smear status - positive')
  names(outcomes_words) <- outcomes
  
  #Define Variables in words
  variables_words <- c('BCG vaccination', 'Years since BCG vaccine', 'Age at vaccination')
  names(variables_words) <- variables
  
  #Define Covariates in words
  covariates_words <- c('Age', 'Ethnic group', 'Sex', 'IMD rank (with 1 as most deprived and 5 as least deprived)', 'Calendar year')
  names(covariates_words) <- covariates
  
  #Define splines in words
  splines_words <- c('age')
  names(splines_words) <- splines
})
