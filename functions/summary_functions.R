

# Construct data tables for a single row of models
data_summary = function(model, variables_words,covariates_words, outcomes_words, data_complete)
{
  
  #Extract key values
  #Pull out key data information
  data <-  flatten(flatten(model[,'data']))[[1]]
  level <-  flatten(flatten(model[,'level']))[[1]]
  outcome <-  flatten(flatten(model[,'outcome']))[[1]]
  variable <-  flatten(flatten(model[,'variable']))[[1]]
  analysis <-  flatten(flatten(model[,'analysis']))[[1]]
  
  #Set outcome and positive outcome
  x <- outcome
  if (x == 'sputsmear')
  {
    Y <- "Positive"
  }else if (x == 'pulmextrapulm') {
    Y <- 'Pulmonary, with or without EP'
    }else{
    Y <- 'Yes'
  }
  
  if (analysis == 'adjusted')
  {
    #construct data summary
    data_summary <- c('Total cases', colnames(data)[-1]) %>% map(function(.){

      ## Assign matching column name
      match_col <- paste0(., 'match')
      
      ## Add total row
      if (. == 'Total cases')
      {
        temp <- c(., nrow(data), paste0(nrow(data[data[[x]] %in% Y,]), ' (', pretty_round(nrow(data[data[[x]] %in% Y,])/nrow(data)*100, digits = 0), ')'), match_col)
      }else if (sum(class(unlist(data[,.])) %in% 'factor'))
      {
        #Add main variable name
        temp <- c(c(variables_words,covariates_words)[.], NA, NA, match_col)
        
        #Add each catagorical variable
        for (i in 1:length(levels(unlist(data[,.]))))
        {
          temp <- rbind(temp, c(paste('&nbsp;&nbsp; ' ,levels(unlist(data[,.]))[i]), 
                                length(unlist(data[unlist(data[,.]) %in% levels(unlist(data[,.]))[i], .])),
                                length(unlist(data[(unlist(data[,x]) %in% Y & unlist(data[,.]) %in% levels(unlist(data[,.]))[i]) , x])),
                                paste0(., levels(unlist(data[,.]))[i])
          ))
          temp[i + 1,3] <- paste(temp[i + 1,3], ' (', pretty_round(as.numeric(temp[i + 1,3])/as.numeric(temp[i + 1,2])*100, digits = 0), ')', collapse = '', sep = '')
        }
      }else if (sum(splines == .) != 0)
      {
        
        #Account for splines/Quadratic terms
        if (level == 'spline')
        {
          level <- 1
          temp <- c(covariates_words[.], NA, NA, match_col)
          
        } else if (level == 'quadratic') {
          #for quadratic
          level <- 2
          temp <- c(covariates_words[.], NA, NA, match_col)
          temp <- rbind(temp, matrix(NA,level,4))
        }else{
          #For linear (i.e no spline)
          temp <- c(covariates_words[.], NA, NA, match_col)
        }
      }else{
        #Account for non catagorical variables
        temp <- c(covariates_words[.], NA, NA, match_col)
      }
      return(temp) })
    
    #Bind and transform data 
    data_summary <- do.call(rbind, data_summary) %>% as_data_frame()
    #Add meaningful column names
    colnames(data_summary) <- c('Demographic Characteristics', 'Total', paste(outcomes_words[outcome],' (%)', collpase = ''),'Variable')
    
  }else{
    data_summary <- NULL
  }
  
  return(data_summary)
}

# Homogenise storage of model summaries
models_summary = function(model)
{
  #FORMAT
  #OR - P-VALUE - LRT

  #Extract key values
  data <-  flatten(flatten(model[,'data']))[[1]]
  level <-  flatten(flatten(model[,'level']))[[1]]
  outcome <-  flatten(flatten(model[,'outcome']))[[1]]
  variable <-  flatten(flatten(model[,'variable']))[[1]]
  analysis <-  flatten(flatten(model[,'analysis']))[[1]]
  model_fit <- flatten(flatten(model[,'glm.summary']))
  LRT <-  flatten(flatten(model[,'anova']))
  #detect if adjusted or univariable and strip out models - additionally store sensitivity anaylsis
  if (analysis == 'univariable')
  {
    model_fit <- do.call(rbind, model_fit) %>% as_data_frame %>% list
  }else{
    model_fit <- model_fit %>% map(as_data_frame) 
  }
  
  model$glm.summary <- list(model_fit)
  return(model)
}

#Bind LRT and glm summary together - as length of each variable is unknown I have had to refer to glm and LRT recursively
# This requires the use of global variables. WARNING!
bind_glm_LRT <- function(glm, LRT, df, level, splines, analysis)
{
  ## drop additional levels from the data frame - to prevent spurious output
  df <- df %>% droplevels

  glm_summary <- lapply(1:length(colnames(df)[-1]), function(j){

    #Preallocate
    temp <- data_frame(Variables = NA, OR = NA, p.value = NA, LRT = NA)[-1,]
    temp$Variables <- as.character(temp$Variables)
    
    #index
    i <- colnames(df)[j + 1]
    
    ##match columns
    match_col <- paste0(i ,'match')
    
    #split off factor variables
    if (sum(class(df[,i][[1]]) %in% 'factor') == 1)
    {
      #Add summary for catagorical P value
      temp_factor <- c(rep(NA, ncol(glm)), LRT[[1]])
      temp_factor[1] <- match_col
      
      #Add line for baseline
      temp_factor <- rbind(temp_factor, temp_factor)
      temp_factor[2,ncol(temp_factor) - 2] <- '1'
      temp_factor[2,ncol(temp_factor)] <- NA
      temp_factor[2,1] <- paste0(i,levels(unlist(df[,i]))[1])
      colnames(temp_factor) <- c('Variables', 'OR', 'p.value', 'LRT')
      temp_factor <- temp_factor %>% as_data_frame()
      
      #Add glm results
      glm_temp <- glm[1:(length(levels(unlist(df[,i]))) - 1),]
      glm_temp <- cbind(glm_temp, LRT = NA) %>% as_data_frame()
      
      
      #Add glm results for catagorical variables
      temp <- bind_rows(temp_factor,  glm_temp)
      
      #update glm and LRT
      glm <<- glm[-(1:(length(levels(unlist(df[,i]))) - 1)),]
      LRT <<- LRT[-1]
    }else if (sum(splines == i) != 0) {
      if (level == 'spline')
      {
        level <- 4 
        ## Spline is not interpretable therefore drop glm results
        #Add summary for catagorical P value
        temp_factor <- t(as.matrix(c(rep('NA', ncol(glm)), LRT[[1]])))
        temp_factor[1,1] <- match_col
        colnames(temp_factor) <- c('Variables', 'OR', 'p.value', 'LRT')
        temp_factor <- temp_factor %>% as_data_frame
        
        #Add glm results for catagorical variables
        temp <- temp_factor
        
        #update glm and LRT
        glm <<- glm[-(1:level),]
        LRT <<- LRT[-1]
      }else if (level == 'quadratic'){
        level <- 2
        #Add summary for catagorical P value
        temp_factor <- t(as.matrix(c(rep('NA', ncol(glm)), LRT[[1]])))
        temp_factor[1,1] <- match_col
        
        colnames(temp_factor) <- c('Variables', 'OR', 'p.value', 'LRT')
        temp_factor <- temp_factor  %>% as_data_frame()
        
        #Add glm results
        glm_temp <- glm[1:level,]
        glm_temp <- cbind(glm_temp, LRT = NA) %>% as_data_frame()
        
        
        #Add glm results for catagorical variables
        temp <- temp_factor %>% bind_rows(glm_temp)
        
        #update glm and LRT
        glm <<- glm[-(1:level),]
        LRT <<- LRT[-1]
      }else{
        temp_factor <- c(match_col, glm[1,-1], LRT[[1]]) %>% as.matrix %>% t
        colnames(temp_factor) <- c('Variables', 'OR', 'p.value', 'LRT')
        temp_factor <- temp_factor %>% as_data_frame
        
        temp <- temp_factor
        
        #update glm and LRT
        glm <<- glm[-1, ]
        LRT <<- LRT[-1]
      }
      
      
    }else{
      temp_factor <- c(match_col, glm[1,-1], LRT[[1]]) %>% as.matrix %>% t
      colnames(temp_factor) <- c('Variables', 'OR', 'p.value', 'LRT')
      temp <- temp_factor %>% as_data_frame
      
      
      #update glm and LRT
      glm <<- glm[-1, ]
      LRT <<- LRT[-1]
    }
    
    #drop p values
    temp <- temp[, !(colnames(temp) %in% 'p.value')]
    
    ## bind identifier rows
    temp <- temp %>% dplyr::mutate(Variable = rep(i, nrow(temp)))
    return(temp)
  })

  ## Bind output
  glm_summary <- do.call(rbind, glm_summary)
  
  ## Add emtpy row
  empty_row <- glm_summary[1,]
  empty_row[1,1] <- 'Total casesmatch'
  empty_row[1,-1] <- NA
  
  glm_summary <- bind_rows(empty_row, glm_summary)
 
  ##format match
  glm_summary <- glm_summary %>%
                    mutate(Variables = Variables %>% as.vector %>% as.character)
  
  if (analysis == 'adjusted') {
    colnames(glm_summary) <- c('match', 'aOR', 'P-value', 'Variables')
  }else{
    colnames(glm_summary) <- c('match', 'OR', 'P-value', 'Variables')
  }
 return(glm_summary)   
}

#Tabulate results in summary tables for a single variable and outcome
summary_tables <- function(models, splines)
{
  
  #Go through all models and summarise table - coded explicitly for an analysis with a univariable and single adjusted analysis
  temp <- lapply(seq(2,nrow(models),2), function(i){

    # Extract key values ------------------------------------------------------
    #data - full
    data <- flatten(flatten(models[i,'data']))[[1]]
    #data - summary
    data_summary <- flatten(models[i,])$data_summary
    
    #Model Summaries
    glm_uni <- flatten(models[i - 1,])$glm.summary[[1]]
    glm_adj <- flatten(models[i,])$glm.summary[[1]]
    
    #Likelihood ratio tests
    LRT_uni <- flatten(models[i - 1,])$anova
    LRT_adj <- flatten(models[i,])$anova
    
    #Analysis level
    level <- flatten(models[i,])$level

    #Reformat LRT and glm results together
    output_uni <- bind_glm_LRT(glm = glm_uni, LRT = LRT_uni, df = data, level, splines,'univariable')
    output_adj <- bind_glm_LRT(glm = glm_adj, LRT = LRT_adj, df = data, level, splines, 'adjusted')

    ## Join glm output together
    output_glm <- dplyr::full_join(output_uni, output_adj, by = 'match') %>% 
                      select(-Variables.x, -Variables.y)
    
    ## Join the data summary and glm output together
    output <- dplyr::left_join(data_summary %>% dplyr::rename(match = Variable), output_glm, by = 'match') %>% 
      select(-match)
    

    ## check join and replace if failing - work around bug in full_join assignment, check if zero in data column (i.e no data) and drop if this is true
    issue_summary <- dplyr::anti_join(data_summary %>% dplyr::rename(match = Variable), output_glm, by = 'match') %>% 
                      select(-match)
    
    if (nrow(issue_summary) > 1) {

      # Find entries that should have no modelling component due to a lack of data 
      DataIssueSum <-   issue_summary[,3] %>% 
                          unlist %>% 
                            str_match_all("[0-9]+") %>% 
                              map_chr(1) %>% 
                                as.numeric
      # Drop rows with no data
      issue_summary <- issue_summary %>% 
                              mutate(NoData = DataIssueSum) %>% 
                                  filter(NoData > 0) %>% 
                                    select(-NoData)
    }
                 
    
    issue_fit <- dplyr::anti_join(output_glm, data_summary %>% dplyr::rename(match = Variable), by = 'match') %>% 
      select(-match)
    
    if (nrow(issue_summary) > 0 && nrow(issue_fit) > 0) {
     
      issue <- bind_cols(issue_summary, issue_fit)
      
      output[output$`Demographic Characteristics` %in% issue$`Demographic Characteristics`, ] <- issue
      
    }

    ## correct column names
    colnames(output) <- c("Demographic Characteristics", "Total",  "Outcome  (%)", "OR", "P-value", "aOR", "P-value1")
    
    return(output)
  })
  
  temp <- c(rbind(list(rep(NULL, length(temp))),temp))
  
  models <- models %>% mutate(output_summary = temp)
  return(models)
}

## String split saved OR summaries
#String split and extract OR and CI
split_extract_OR_CI = function(fit) {
  effect_var <- fit %>%  strsplit("[[:space:]]|(?=[^.'[:^punct:]])", perl = TRUE) %>% unlist()
  effect_var <- effect_var[!effect_var %in% ""]
  OR <- effect_var[1] %>% as.numeric()
  UCI <- effect_var[5] %>% as.numeric()
  LCI <- effect_var[3] %>% as.numeric()
  
  return(list(OR, UCI, LCI))
}

#Extract the main variable effect and CI with p value 
main_effect <- function(model)
{
  #Extract model summary
  model_fit <- flatten(model[,]$glm.summary)[[1]]
  
  SplitOR <- split_extract_OR_CI(model_fit$OR[1])
  
  OR <- SplitOR[[1]]
  UCI <- SplitOR[[2]]
  LCI <- SplitOR[[3]]
  
  #Extrat confidence intervals
  p.value <- model_fit$p.value[1]
  
  model <- model %>% mutate(OR = OR, UCI = UCI, LCI = LCI, p.value = p.value)
}

#Extract the secondary variable effect and CI with p value 
secondary_effect <- function(model, data)
{
  #Extract key values
  level <-  flatten(flatten(model[,'level']))[[1]]
  outcome <-  flatten(flatten(model[,'outcome']))[[1]]
  variable <-  flatten(flatten(model[,'variable']))[[1]]
  analysis <-  flatten(flatten(model[,'analysis']))[[1]]
  anova <- flatten(flatten(model[,'anova']))[[1]]
  #Extract model summary
  model_fit <- flatten(model[,]$glm.summary)[[1]]
  
  #detect levels in the data
  levels_data <- with(data, levels(get(variable)))[-1]
  
  #Calculate effect sizes
  temp <- lapply(1:length(levels_data), function(i){
    #String split and extract OR and CI
    SplitOR <- split_extract_OR_CI(model_fit$OR[i])
    
    OR <- SplitOR[[1]]
    UCI <- SplitOR[[2]]
    LCI <- SplitOR[[3]]
    
    #Extract confidence intervals
    p.value <- model_fit$p.value[i]
    
    temp <- data_frame(outcome = outcome, variable = variable, analysis = analysis, 
                       level = level, category = levels_data[i],
                       OR = OR, UCI = UCI, LCI = LCI, p.value = p.value, LRT = anova)
  }) %>% bind_rows
    
  
}

#Extract the main variable effect and CI with p value 
sensitivity_summary <- function(model)
{
  
  #Extract key values
  level <-  flatten(flatten(model[,'level']))[[1]]
  outcome <-  flatten(flatten(model[,'outcome']))[[1]]
  variable <-  flatten(flatten(model[,'variable']))[[1]]
  analysis <-  flatten(flatten(model[,'analysis']))[[1]]
  model_fit <-  flatten(flatten(model[,'glm.summary']))
  data <- model[['data']][[1]]$full

  if (analysis == 'adjusted') {
    
    if (class(uk_not[[variable]]) == 'factor') {
      #detect levels in the data
      levels_data <- levels(data[[variable]])[-1]
    }else{
      levels_data <- NA
    }

    #Find effect of variable for each sensitivity analysis
    cov.sensitivity <- model_fit[-c(2)] %>% map_df(function(., level_var = levels_data){
         temp <- lapply(1:length(levels_data), function(i, level_var = levels_data){
           #String split and extract OR and CI
           SplitOR <- split_extract_OR_CI(.$OR[i])
           
           OR <- SplitOR[[1]]
           UCI <- SplitOR[[2]]
           LCI <- SplitOR[[3]]
           #Extract p values
           p.value <- .$p.value[i]
           
           #store OR, CI, p values
           temp <- data_frame(outcome, variable, OR, LCI, UCI, p.value = p.value, category = level_var[i])
           return(temp)
         }) %>% bind_rows
    }) 

    #Add covariate names
    cov.sensitivity <- cov.sensitivity %>% 
                        mutate(covariate = map(c('adjusted', names(model_fit)[-c(1:2)]), function(.) rep(., length(levels_data))) %>% unlist)
  }else{
    #Add blank storage
    cov.sensitivity <- NULL
  }
  
  model <- model %>%
      mutate(cov.sensitivity = list(cov.sensitivity))
}

#Plot summary table of all outcomes against a single variable
out_var_summary <- function(model, outcomes_words)
{

  
  #Bind OR and CI into a single column
  model <- model %>% mutate(OR = paste(format(OR, nsmall = 2), ' (', format(LCI, nsmall = 2), ' to ', format(UCI, nsmall = 2), ')', sep = ''))
  #Seperate summary statistics
  model_uni <- model[model$analysis %in% 'univariable', c('OR', 'p.value') ]
  model_adj <- model[model$analysis %in% 'adjusted', c('OR', 'p.value') ]
  
  #Produce summary table
  outcome <- as_data_frame(outcomes_words)
  model <- bind_cols(outcome, model_uni, model_adj)
  colnames(model) <- c('Outcome', 'OR', 'P-value', 'aOR', 'P-value1')
  return(model)
}

#Plot summary table of all outcomes against a single variable - for a categorical variable
out_var_summary_cat <- function(model, outcomes_words, var_cat)
{

  #Bind OR and CI into a single column
  model <- model %>% mutate(OR = paste(format(OR, nsmall = 2), ' (', format(LCI, nsmall = 2), ' to ', format(UCI, nsmall = 2), ')', sep = ''))
  
  #Bind table for univariable and adjusted
  model <- bind_cols(model[model$analysis %in% 'univariable', c('outcome', 'category', 'OR', 'p.value', 'LRT')], model[model$analysis %in% 'adjusted', c('OR', 'p.value', 'LRT')])
  colnames(model) <- c('Outcome', 'Category', 'OR', 'p value','LRT', 'aOR', 'ap value', 'aLRT')
  
  #Remove additional outcome columns and add baseline
  model <- unique(model$Outcome) %>% map_df(function(i){
   . <- model[model$Outcome %in% i,]
   LRT <- .$LRT[1]
   aLRT <- .$aLRT[1]
   .$Outcome <- ''
   .$LRT <- ''
   .$aLRT <- ''
   
   . <- bind_rows(data_frame(Outcome = outcomes_words[i], Category = var_cat[1], OR = '1', `p value` = '', LRT = LRT, aOR = '1', `ap value` = '', aLRT = aLRT), .)
   return(.)
  })

  model <- model %>% 
              select(-`p value`, -`ap value`)
  
  colnames(model) <- c('Outcome', 'Age group (years)', 'OR', 'P-value', 'aOR', 'P-value1')
 
  return(model)
}

## Summarise the total cases, cases with outcome and cases without outcome
sum_sample_size_single_analysis = function(Variable, Outcome, df) {

  ## count variables and outcomes in dataset replace variables that have missing entries with zero entries
  dots <- lapply(c(Variable, Outcome), as.symbol)
  Cases <- df %>% 
            dplyr::count_(dots) %>%
              rename_(Outcome = Outcome, Variable = Variable) %>%
                tidyr::complete(Outcome) %>%
                  mutate(n = n %>% replace(is.na(n), 0)) %>% 
                    spread(key = Outcome, value = n)
  
  colnames(Cases) <- c('Variable', 'CasesWithoutOutcome', 'CasesWithOutcome')
  
  Cases <- Cases %>% 
              mutate(Cases =  rowSums(.[c("CasesWithoutOutcome", "CasesWithOutcome")], na.rm = T)) %>% 
                ungroup %>% 
                  select(-Variable)
  
  return(Cases)
}


## Summarise sample for a single outcome and all levels
sum_sample_size_strat_exp = function(Outcome, Variable, Covariates, df) {

  ## Data with no missing outcomes or exposures
 dfNoMissOutVar <-  df  %>% 
                      filter_(.dots = c(paste0('!is.na(', Variable, ')'), paste0('!is.na(', Outcome, ')')))

 ## data for multivariable analysis
 dfNoMissOutVarCov <- dfNoMissOutVar %>%
                        filter_(.dots = paste0('!is.na(', Covariates, ')'))
 
 ## Summarise Univariable Cases
 UniCases <- sum_sample_size_single_analysis(Variable, Outcome, dfNoMissOutVar)
 colnames(UniCases) <- paste0('Uni_', colnames(UniCases))
 
 ## Summarise Multivariable cases 
MultiCases <- sum_sample_size_single_analysis(Variable, Outcome,  dfNoMissOutVarCov)
colnames(MultiCases) <- paste0('Multi_', colnames(MultiCases))

Cases <- UniCases %>% bind_cols(MultiCases)

return(Cases)
}


#Likelihood ratio test on effect of modelling age
LRT_age_summary <- function(models, levels_var, outcomes_words)
{
  #Extract unique
  variables <- unique(models$variable)
  outcomes <- unique(models$outcome)
  analysis_var <- unique(models$analysis)
  
  #bind LRT results together for all variables and outcomes
  age_LRT_summary <- lapply(1:length(analysis_var), function(j){
    temp <- map2_df(rep(outcomes,length(variables)), sort(rep(variables,length(outcomes))), function(outcome, variable, analysis = analysis_var[j], levels = levels_var){
      #select models to compare
      nested_models <- models[models$variable %in% variable & models$outcome %in% outcome & models$analysis %in% analysis & models$level %in% levels,]
      
      #list models
      nested_models <- lapply(1:nrow(nested_models), function(i){
        if (analysis %in% 'univariable')
        {
          temp <- flatten(nested_models[i,])$glm$age
        }else{
          temp <- flatten(nested_models[i,])$glm$full
        }
        return(temp)
      })
      
      #reformat list class
      class(nested_models) <- 'glmlist'
      anova_age <- anova(nested_models, test = 'LRT')
      
      age_LRT_summary <- data_frame(outcomes = outcomes_words[outcome], variable = variable, level.1 = levels[1], level.2 = levels[2], 
                                    analysis = analysis, Df = anova_age$Df[-1], Deviance = signif(anova_age$Deviance[-1], digits = 3),
                                    `Pr(>Chi)` = ifelse(anova_age$`Pr(>Chi)`[-1] < 0.001, '<0.001',as.character(signif(anova_age$`Pr(>Chi)`[-1],digits = 3))))
    })
    return(temp)
  }) %>% bind_rows()

}

#table that generates proptions and tests via prop.test
improved_table = function(var_1, var_2)
{
  t <- table(var_1, var_2)
  
#compare each row
  t <- cbind(t, lapply(1:nrow(t), function(j){
    temp <- apply(t[,1:2],2,as.numeric)
    temp1 <- prop.test(c(temp[j,1], temp[j,2]), rep(sum(temp[j,1:2]),2)) 
    temp <- c(round(temp1$estimate*100, digits = 1), ifelse(temp1$p.value < 0.001, '< 0.001', as.character(round(temp1$p.value, digits = 3))), sum(temp[j,1:2])) 
  }) %>% unlist %>% matrix(4,nrow(t)) %>% t)
  
 #compare prop for column 
  t <- cbind(t, lapply(1:nrow(t), function(j){
    temp <- apply(t[,1:2],2,as.numeric)
    temp <- prop.test(c(temp[j,1], temp[j,2]), colSums(temp[,1:2])) 
    temp <- c(round(temp$estimate*100, digits = 1), ifelse(temp$p.value < 0.001, '< 0.001', as.character(round(temp$p.value, digits = 3)))) 
  }) %>% unlist %>% matrix(3,nrow(t)) %>% t)

  #summarise rows
  t <- rbind(t, c(colSums(apply(t[,1:2],2,as.numeric)), rep('', ncol(t) - 2)))
  
  return(t)
}
