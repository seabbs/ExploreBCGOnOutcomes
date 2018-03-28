## Run analysis for imputed data set, aggregating results into a single table
  bake(file = paste0('../data/', folder, '/imputed_sum_tab.rds'), {
    # wrapper function --------------------------------------------------------
    
    fit_imputation_wrapper <- function(imp_df)
    {
      # run and pool models -----------------------------------------------------
      
      # univariable --------------------------------------------------------------
      model_outcome_uni = function(outcome, variable, LRT = FALSE){
        
        if (LRT) {
          formula <- paste0(outcome, '~', '1')
        }else{
          formula <- paste0(outcome, '~', '1 +', variable)
        }
        
        model <- with(imp_df, glm(as.formula(formula), family = binomial("logit")))
        
        model <- pool(model)
        
        return(model)
      }
      
      model_bcgvacc_uni <- outcomes %>% map(function(.) model_outcome_uni(., variable = 'bcgvacc'))
      model_ageatvac_uni <- outcomes %>% map(function(.) model_outcome_uni(., variable = 'ageatvac'))
      model_yrsinceBCG_uni <- outcomes %>% map(function(.) model_outcome_uni(., variable = 'yrsinceBCG'))
      
      
      # Adjusted ----------------------------------------------------------------
      
      model_outcome_adj = function(outcome, variable, LRT = FALSE){
        #covariates with spline - assumes added spline to imputation  
        cov <- covariates %>% map_chr(function(x){
          if (splines == x) {
            x <- paste0('age', seq(1,3)) %>% paste(collapse = ' + ') 
          }else{
            x <- x
          }
          return(x)
        })
        
        if (LRT) {
          formula <- paste0(outcome, '~', "1", paste(c('', cov), collapse = ' + '))
        }else{
          formula <- paste0(outcome, '~', variable, paste(c('', cov), collapse = ' + '))
        }
        
        
        model <- with(imp_df, glm(as.formula(formula), family = binomial("logit")))
        
        model <- pool(model)
        
        return(model)
      }
      
      
      
      model_bcgvacc_adj <- outcomes %>% map(function(.) model_outcome_adj(., variable = 'bcgvacc')) 
      model_ageatvac_adj <- outcomes %>% map(function(.) model_outcome_adj(., variable = 'ageatvac'))
      model_yrsinceBCG_adj <- outcomes %>% map(function(.) model_outcome_adj(., variable = 'yrsinceBCG'))
      
         # Summarise results -------------------------------------------------------

      # BCG vaccination --------------------------------------------------------
      
      #univaraite
      sum_bcg_uni <- model_bcgvacc_uni %>%
                      map(summary)  %>% 
                        map(function(.) .[c(2),c('est', 'lo 95', 'hi 95', 'Pr(>|t|)', 'fmi')]) %>% map(function(.) . <- c(exp(.[c('est', 'lo 95', 'hi 95')]), ifelse(.[c('Pr(>|t|)')] < 0.001, '<0.001', pretty_round(.[c('Pr(>|t|)')] , digits = 3)), pretty_round(.[c('fmi')]*100, digits = 0))) 
      sum_bcg_uni <- do.call(rbind, sum_bcg_uni) %>%
                      as_data_frame %>%
                        mutate(OR = paste0(pretty_round_inf(est),' (', pretty_round_inf(`lo 95`),' to ', pretty_round_inf(`hi 95`),')'), Outcome = outcomes_words) %>% select_(.dots = c('Outcome', 'OR', '`Pr(>|t|)`', 'fmi')) %>% 
                          dplyr::rename(`P-value` = `Pr(>|t|)`)
      
      #adjusted
      sum_bcg_adj <- model_bcgvacc_adj %>% 
                      map(summary)  %>% 
                        map(function(.) .[c(2),c('est', 'lo 95', 'hi 95', 'Pr(>|t|)', 'fmi')]) %>% map(function(.) . <- c(exp(.[c('est', 'lo 95', 'hi 95')]), ifelse(.[c('Pr(>|t|)')] < 0.001, '<0.001', pretty_round(.[c('Pr(>|t|)')] ,digits = 3)), pretty_round(.[c('fmi')]*100,digits = 0))) 
      sum_bcg_adj <- do.call(rbind, sum_bcg_adj) %>% 
                        as_data_frame %>% 
                          mutate(aOR = paste0(pretty_round_inf(est),' (', pretty_round_inf(`lo 95`),' to ', pretty_round_inf(`hi 95`),')'), Outcome = outcomes_words) %>% select_(.dots = c('Outcome', 'aOR', '`Pr(>|t|)`', 'fmi')) %>% 
                            dplyr::rename(`P-value` = `Pr(>|t|)`)
      
      #summary
      sum_imp_bcg <- bind_cols(sum_bcg_uni, sum_bcg_adj[,-1])
      
      # Age at vaccination ------------------------------------------------------
      
      #univariate
      sum_ageatvac_uni <- model_ageatvac_uni %>%
                            map(summary) %>% map(function(.) .[c(2:4),c('est', 'lo 95', 'hi 95', 'Pr(>|t|)', 'fmi')])  %>% map(function(.) . <- cbind(exp(.[,c('est', 'lo 95', 'hi 95')]), ifelse(.[,c('Pr(>|t|)')] < 0.001, '<0.001', pretty_round(.[,c('Pr(>|t|)')] ,digits = 3)), pretty_round(.[,c('fmi')]*100,digits = 0))) 
      
      sum_ageatvac_uni <- do.call(rbind, sum_ageatvac_uni) %>% 
                            as_data_frame %>% 
                              plyr::mutate(Outcome = unlist(outcomes_words %>% map(function(.) c(.,rep('',2)))))
      colnames(sum_ageatvac_uni) <- c('est', 'lo 95', 'hi 95', 'Pr(>|t|)', 'fmi', 'Outcome')
      sum_ageatvac_uni <- sum_ageatvac_uni %>%
                            mutate(OR = paste0(pretty_round_inf(est),' (', pretty_round_inf(`lo 95`),' to ', pretty_round_inf(`hi 95`),')'), Category = rep(levels(uk_not$ageatvac)[-1], length(outcomes))) %>% select_(.dots = c('Outcome','Category', 'OR', '`Pr(>|t|)`', 'fmi')) %>% 
                              dplyr::rename(`P-value` = `Pr(>|t|)`, `Age group (years)` = Category)
      
      #adjusted
      sum_ageatvac_adj <- model_ageatvac_adj %>% 
                            map(summary) %>% map(function(.) .[c(2:4),c('est', 'lo 95', 'hi 95', 'Pr(>|t|)', 'fmi')])  %>%
                              map(function(.) . <- cbind(exp(.[,c('est', 'lo 95', 'hi 95')]), ifelse(.[,c('Pr(>|t|)')] < 0.001, '<0.001', pretty_round(.[,c('Pr(>|t|)')] ,digits = 3)), pretty_round(.[,c('fmi')]*100, digits = 0))) 
      sum_ageatvac_adj <- do.call(rbind, sum_ageatvac_adj) %>%
                            as_data_frame %>% 
                              plyr::mutate(Outcome = unlist(outcomes_words %>% map(function(.) c(.,rep('',2)))))
      colnames(sum_ageatvac_adj) <- c('est', 'lo 95', 'hi 95', 'Pr(>|t|)', 'fmi', 'Outcome')
      sum_ageatvac_adj <- sum_ageatvac_adj %>%
                            mutate(aOR = paste0(pretty_round_inf(est),' (', pretty_round_inf(`lo 95`),' to ', pretty_round_inf(`hi 95`),')'), Category = rep(levels(uk_not$ageatvac)[-1], length(outcomes))) %>%
                              select_(.dots = c('Outcome', 'aOR', '`Pr(>|t|)`', 'fmi')) %>% 
                                dplyr::rename(`P-value` = `Pr(>|t|)`)
      
      #summary
      sum_imp_ageatvac <- bind_cols(sum_ageatvac_uni, sum_ageatvac_adj[,-c(1)])
      
      # Year since Vaccination -------------------------------------------------
      
      #univaraite
      sum_yrsinceBCG_uni <- model_yrsinceBCG_uni %>% 
                              map(summary)  %>% 
                                map(function(.) .[c(2),c('est', 'lo 95', 'hi 95', 'Pr(>|t|)', 'fmi')]) %>% map(function(.) . <- c(exp(.[c('est', 'lo 95', 'hi 95')]), ifelse(.[c('Pr(>|t|)')] < 0.001, '<0.001', pretty_round(.[c('Pr(>|t|)')] ,digits = 3)), pretty_round(.[c('fmi')]*100,digits = 0))) 
      sum_yrsinceBCG_uni <- do.call(rbind, sum_yrsinceBCG_uni) %>% 
                              as_data_frame %>% 
                                mutate(OR = paste0(pretty_round_inf(est),' (', pretty_round_inf(`lo 95`),' to ', pretty_round_inf(`hi 95`),')'), Outcome = outcomes_words) %>% 
                                  select_(.dots = c('Outcome', 'OR', '`Pr(>|t|)`', 'fmi')) %>% 
                                    dplyr::rename(`P-value` = `Pr(>|t|)`)
      
      #adjusted
      sum_yrsinceBCG_adj <- model_yrsinceBCG_adj %>%
                              map(summary)  %>%
                                map(function(.) .[c(2),c('est', 'lo 95', 'hi 95', 'Pr(>|t|)', 'fmi')]) %>% map(function(.) . <- c(exp(.[c('est', 'lo 95', 'hi 95')]), ifelse(.[c('Pr(>|t|)')] < 0.001, '<0.001',pretty_round(.[c('Pr(>|t|)')] ,digits = 3)), pretty_round(.[c('fmi')]*100,digits = 0))) 
      sum_yrsinceBCG_adj <- do.call(rbind, sum_yrsinceBCG_adj) %>% 
                              as_data_frame %>% 
                                mutate(aOR = paste0(pretty_round_inf(est),' (', pretty_round_inf(`lo 95`),' to ', pretty_round_inf(`hi 95`),')'), Outcome = outcomes_words) %>%
                                  select_(.dots = c('Outcome', 'aOR', '`Pr(>|t|)`', 'fmi')) %>% 
                                    dplyr::rename(`P-value` = `Pr(>|t|)`)
      
      #summary
      sum_imp_yrsinceBCG <- bind_cols(sum_yrsinceBCG_uni, sum_yrsinceBCG_adj[,-1])
      
      ## Rename p value columns
      
      return(list(sum_imp_bcg, sum_imp_ageatvac, sum_imp_yrsinceBCG))
    }
    
    output <- fit_imputation_wrapper(imp_df)
  }) -> output
  
  sum_imp_bcg <- output[[1]]
  
  sum_imp_ageatvac <- output[[2]]
  
  sum_imp_yrsinceBCG <- output[[3]] 
  