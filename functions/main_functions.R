
# Run regression analysis -------------------------------------------------
main_analysis <- function(outcomes, variables, covariates, splines, data, BF_cor, BF_scale)
{
  #Construct data frame of all models and there required data sets
  models <-  map2(rep(outcomes,length(variables)), sort(rep(variables,length(outcomes))),
                  function(outcome, variable) models_comb(outcome, variable, covariates) %>%
                    models_data(data, variable, covariates)) %>% do.call(rbind,.) %>% models_flash(c('linear', 'spline'))
  
  #Run Regressions and calculate anova for each model
  models <- models %>% models_regress(splines, BF_cor, BF_scale)
  
  #Search for regression issues
  model_warnings <- flatten(models[,'warnings']) %>% 
                      map_dbl(function(.)if (!is.na(.)) . <- 1 else . <- 0) %>% 
                         as.logical %>% 
                            (function(x) {models[x,]})
  
  if (nrow(model_warnings) != 0 ) print(model_warnings)
  
  return(list(models,model_warnings))
}

# Summarise Analysis ------------------------------------------------------
main_summary <- function(models, outcomes_words, variables_words, covariates_words, splines, data)
{

  #Construct Output summary tables - add demographic data
  data_summary <- lapply(1:nrow(models), function(i){models[i,]}) %>% map( function(.) data_summary(., variables_words, covariates_words, outcomes_words, data))
  models <- models %>% mutate(data_summary = data_summary)

  #Summarise ORs for each model
  models <-  lapply(1:nrow(models), function(i){models[i,]}) %>% map_df(models_summary)
  
  #Concatinate final output in a single table for each outcome and variable combination
  models <- models %>% summary_tables(splines)
  
  #Summarise results for each variable against all outcomes
  models <-  lapply(1:nrow(models), function(i){models[i,]}) %>% map_df(main_effect)

  #Summarise the secondary effects
  secondary_summary <-  lapply(1:nrow(models), function(i){models[i,]}) %>% map_df(function(.,df=data) secondary_effect(., df))
  
  #Summarise sensitivity analysis for each variable against all outcomes
  models <-  lapply(1:nrow(models), function(i){models[i,]}) %>% map_df(sensitivity_summary)

  #Bin predicted and observed data by age and store in single data frame - to allow for model plotting
  data_predict <- list()
  #data_predict <- lapply(1:nrow(models), function(i){models[i,]}) %>% map(quietly(function(.)data_binned_age(.,data))) %>% transpose()
  
  data_pred_warn <- list()
  #data_pred_warn <- data_predict$warnings
  #Flag warnings
  ##if (sum(map_dbl(data_pred_warn,length)) != 0) warning('Sample size in age binning is to small to return an accurate estimate')
  
  #Store and name results
  #data_predict <- do.call(rbind, data_predict$result)
  
  #Test age variable complexity
  levels <- list(c('linear', 'quadratic'), c('quadratic', 'spline'), c('linear', 'spline'))
  age_LRT <- levels %>% map_df(function(.) LRT_age_summary(models, ., outcomes_words))
  
  return(list(models, data_predict, data_pred_warn, age_LRT, secondary_summary))
}

# Run and Summarise analysis ----------------------------------------------
regression_analysis = function(outcomes, variables, covariates, splines, outcomes_words, variables_words, covariates_words, splines_words, uk_not, BF_cor = FALSE, BF_scale = NULL)
{

  # Run the Analysis --------------------------------------------------------
  #switch on JIT
  enableJIT(3)
  
  temp <- main_analysis(outcomes, variables, covariates, splines, uk_not, BF_cor, BF_scale)
  
  #Store tibble of output and models run
  models <- temp[[1]]
  
  #store model warnings
  models_warnings <- temp[[2]]

  # Summarise Analysis ------------------------------------------------------
  temp <- main_summary(models, outcomes_words, variables_words, covariates_words, splines, uk_not)

  #Store updated tibble containing summary information
  models <- temp[[1]]
  
  #store predicted data binned by age
  data_predict <- temp[[2]]
  
  #store warnigns for predicted data binned by age
  data_pred_warn <- temp[[3]]
  
  #store LRT tests for age variable
  age_LRT <- temp[[4]]
  
  #Store the secondary effect
  secondary_effect_summary <- temp[[5]]
  
  rm(temp) 
  
  # Test spline with LRT ----------------------------------------------------
  level_compare <- list(c('linear', 'spline'))
  table_age_LRT <- list()  
  for (i in variables)
  {
    temp_t <- list()
    for (j in level_compare)
    {
      temp_t[[paste(j, collapse = ' ')]] <- age_LRT %>% dplyr::filter(variable == i & level.1 == j[1] & level.2 == j[2]) %>%
        dplyr::select(outcomes, analysis, `Pr(>Chi)`) %>%
        spread(analysis, `Pr(>Chi)`) %>% dplyr::select(outcomes, univariable, adjusted) 
    }
    table_age_LRT[[i]] <- temp_t
  }
  
  # Main effect -------------------------------------------------------------
  
  #Plot summary tables for variable against outcomes - just reporting models fitted with splines
  table_var_outs <- list()
  ModelDataSum <- list()
  
  for (i in variables) 
  {
    j <- 'spline'
    
      model_var <- secondary_effect_summary[secondary_effect_summary$level %in% j & secondary_effect_summary$variable %in% i,]
      
      #Summarise in readable format
      variable_summary <- out_var_summary_cat(model = model_var, outcomes_words, var_cat = levels(uk_not[[i]]))
      
      ## Sample size summaries by variable across all models
      ModelDataSum[[i]] <-  outcomes %>% 
                            map(~sum_sample_size_strat_exp(., Variable = i, Covariates = covariates, df = uk_not)) %>% 
                              bind_rows
    
 ##   }
    table_var_outs[[i]] <- variable_summary
  }
  
  # Full results ------------------------------------------------------------
  #Tables for each variable and outcomes
  table_var_out <- list()
  
  for (i in variables) 
  {
    # temp_p <- list()
    temp_d <- list()
    
    for (j in outcomes) {
      #print summary tables
      temp1_d <- list()
      for (h in c('spline'))
      {
        output_var_out <- models[models$variable %in% i & models$outcome %in% j & models$level %in% h & models$analysis %in% 'adjusted', ]$output_summary
        #Reformat NA values
        output_var_out <- output_var_out[[1]]
        output_var_out <- output_var_out %>% map(function(.) ifelse(is.na(.), '', .))  %>% 
          map(function(.) ifelse(. == 'NA', '', .)) 
        output_var_out <- do.call(cbind, output_var_out)
        
        temp1_d[[h]] <- as.data.frame(output_var_out)
      }
      
      temp_d[[j]] <- temp1_d
    }
  table_var_out[[i]] <- temp_d 
}
    

  #save all analysis output in data  stripped format
  output <- list(table_var_out, table_var_outs, table_age_LRT, ModelDataSum)
  
 return(output) 
}
