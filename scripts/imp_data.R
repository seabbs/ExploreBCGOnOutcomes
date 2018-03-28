# For nested variables need to add level to make not nested ----------------
if(sum(imputed_var %in% 'TB_mortality') == 1 )
{
  uk_not_nonested$TB_mortality <- uk_not_nonested$TB_mortality %>% as.character
  uk_not_nonested$TB_mortality <- ifelse(uk_not_nonested$Mortality %in% 'No', 'Still Alive', uk_not_nonested$DeathDueTB) %>%
    factor
}


## Handle the nesting of yrsinceBCG in BCG vaccination (i.e assign NA to all yrsinceBCG cases where a case has not actually had BCG)
if(sum(imputed_var %in% 'yrsinceBCG') == 1 )
{
  uk_not_nonested$yrsinceBCG <- uk_not_nonested$yrsinceBCG %>% as.character
  uk_not_nonested$yrsinceBCG <- ifelse(uk_not_nonested$bcgvacc %in% 'No', 'Not vac', uk_not_nonested$yrsinceBCG) %>%
    factor
}
## Handle the nesting of ageatvac in BCG vaccination (i.e assign NA to all ageatvac cases where a case has not actually had BCG)
if(sum(imputed_var %in% 'ageatvac') == 1 )
{
  uk_not_nonested$ageatvac <- uk_not_nonested$ageatvac %>% as.character
  uk_not_nonested$ageatvac <- ifelse(uk_not_nonested$bcgvacc %in% 'No', 'Not vac', uk_not_nonested$ageatvac) %>% 
    factor
}

## add spline relationship to imputation model
if (sum(imputed_var %in% 'age') == 1 ) {
  uk_not_nonested <- uk_not_nonested %>% 
                              mutate(age1  = ns(age, knots = c(summary(age)[c("1st Qu.","Median", "3rd Qu.")]))[,1],
                                    age2 =  ns(age, knots = c(summary(age)[c("1st Qu.","Median", "3rd Qu.")]))[,2], 
                                    age3 =  ns(age, knots = c(summary(age)[c("1st Qu.","Median", "3rd Qu.")]))[,3])
  imputed_var <- c(imputed_var, 'age1', 'age2', 'age3')
  imputed_var <- imputed_var[imputed_var != 'age']
}

## Maxit set to 50 iterations
miss_per_var <- uk_not_nonested %>%  map_dbl(function(.) sum(is.na(.))/length(.)*100) %>% mean
## set as per of missing data m = 30 

stew(file = paste0(direct_data, folder, '/imputed_data.rda'), {
  # initialise and turn off impute for age and year since vac ---------------
  imp_df <- mice(data = uk_not_nonested[, imputed_var], maxit = 0)
  
  meth <- imp_df$method
  
  ## prediction model excluding variables from imputing from each other that have the same missing characterisitic (as constructed from overall outcome)
  pred <- imp_df$predictorMatrix
  pred[c('Mortality', 'TB_mortality'), c('Mortality', 'TB_mortality')] <- 0
  
  visit <- imp_df$visitSequence
  
  # Impute data -------------------------------------------------------------
  imp_df <- mice(data = uk_not_nonested[, imputed_var], maxit = 20, m = 50, predictorMatrix = pred, method = meth, visitSequence = visit)
})

## Imputed data plots - check convergance and distribution ets

## trace plot
#plot(imp_df)

## distributions of exposure variables
#bcg vacc
#densityplot(imp_df, ~bcgvacc)

## Yrs since vac
#densityplot(imp_df, ~yrsinceBCG)

## Age at vac
#densityplot(imp_df, ~ageatvac)

## Distributions of outcomes
#Mortality
#densityplot(imp_df, ~Mortality)

#TB mortality
#densityplot(imp_df, ~DeathDueTB)

#Successful treatment
#densityplot(imp_df, ~SuccTreat)

#Drug resistance
#densityplot(imp_df, ~anyres)

#Pulmonary TB
#densityplot(imp_df, ~pulmextrapulm)

#Sputum smear
#densityplot(imp_df, ~sputsmear)

## Handle the nesting of TB mortality in mortality (i.e assign NA to all TB mortatlity cases where a case has not actually died)
stew(file = paste0(direct_data, folder, '/imputed_data_reform.rda'), {
## format to long format for manipulation
imp_df_long  <- complete(imp_df, action = 'long', include = TRUE) 
if (sum(imputed_var %in% 'TB_mortality') == 1 )
{
  imp_df_long$TB_mortality <- imp_df_long$TB_mortality %>% as.character
  imp_df_long$TB_mortality <- ifelse(imp_df_long$TB_mortality %in% 'Still Alive', NA, imp_df_long$TB_mortality) %>%
    factor %>% 
      droplevels
}


## Handle the nesting of yrsinceBCG in BCG vaccination (i.e assign NA to all yrsinceBCG cases where a case has not actually had BCG)
if (sum(imputed_var %in% 'yrsinceBCG') == 1 )
{
  imp_df_long$yrsinceBCG <- imp_df_long$yrsinceBCG %>% as.character
  imp_df_long$yrsinceBCG <- ifelse(imp_df_long$yrsinceBCG  %in% 'Not vac', NA, imp_df_long$yrsinceBCG) %>%
    factor %>% 
       droplevels
}
## Handle the nesting of ageatvac in BCG vaccination (i.e assign NA to all ageatvac cases where a case has not actually had BCG)
if (sum(imputed_var %in% 'ageatvac') == 1 )
{
  imp_df_long$ageatvac <- imp_df_long$ageatvac %>% as.character
  imp_df_long$ageatvac <- ifelse(imp_df_long$ageatvac %in% 'Not vac', NA, imp_df_long$ageatvac) %>% 
    factor %>% 
      droplevels
}

## reformat as imputed data
imp_df <- as.mids(imp_df_long)
})