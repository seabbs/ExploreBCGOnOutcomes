# Define funcitons for analysis -------------------------------------------

# Define Models -----------------------------------------------------------
models_comb <- function(outcome, variable, covariates)
{
  univariable = c(list(c(outcome,variable)),covariates %>% map(function(x)
    c(outcome, x)))
  names(univariable) <- c(variable, covariates)
  
  adjusted = c(list(c(
    outcome, variable, covariates
  )), c(variable, covariates) %>% map(
    function(x)
      c(outcome, variable, covariates)[!(c(outcome, variable, covariates) %in% x)]
  ))
  names(adjusted) <- c('full', variable, covariates)
  
  
  models <- tibble::frame_data(
    ~outcome, ~variable, ~analysis,    ~models,
    outcome, variable, 'univariable', univariable,
    outcome, variable, 'adjusted',   adjusted
  )
  return(models)
}

# Flash models to allow of mulitple analysis levels (i.e linear, quadratic, splines)
models_flash <- function(models, levels)
{
  #store temp models
  temp_models <- models
  row_models <- nrow(models)
  #flash rows - crude but tricky otherwise
  if (length(levels) > 1)
  {
    for (i in levels[-1])
    {
      models <- dplyr::bind_rows(models, temp_models)
    }
  }
  
  #define a column indicating analysis level
  models <- models %>% mutate(level = unlist(map(levels, function(.) rep(.,row_models))))
  models <- models[,c(1:3,6, 4:5)]
}

# Add restricted data -----------------------------------------------------
models_data <- function(models, data, variable, covariates)
{

  univariable <- flatten(flatten(models[models$analysis %in% 'univariable', 'models' ])) %>% map(function(x) na.omit(data[, x]))
  names(univariable) <- c(variable, covariates)
  
  adjusted  <-  flatten(flatten(models[models$analysis %in% 'adjusted', 'models' ]))[['full']] %>% list() %>%  map(function(x) na.omit(data[, x])) %>% rep((length(covariates) + 2))
  names(adjusted) <- c('full', variable, covariates)
  
  
  #bind into data frame
  models <- models %>% mutate(data = list(
    univariable,
    adjusted))
  return(models)
}

#Regressions for each outcome and variable ---------------------------
models_regress = function(models, splines = '', BF_cor, BF_scale)
{
  temp <- lapply(1:nrow(models), function(i) {
    
    #Define model formula
    # Replace any variables with splines as specified (knots on the 25%, 50% and 75% quantile) - for spline level
    # Replace any variables with quadratics as specified - for quadratic level
    if (flatten(flatten(models[i, 'level'])) == 'spline')
    {
      
      model_formula <- flatten(flatten(models[i, 'models'])) %>% map(function(.){
        unlist(map(., function(x){
          if (sum(splines == x) == 1)
          {
            x <-
              paste(
                'ns(',x, ', knots=c(summary(', x, ')[c("1st Qu.","Median", "3rd Qu.")]))', collapse =
                  ''
              )
          }else
          {
            x <- x
          }
        }))
      })
    }else if(flatten(flatten(models[i, 'level'])) == 'quadratic')
    {
      model_formula <- flatten(flatten(models[i, 'models'])) %>% map(function(.){
        unlist(map(., function(x){
          if (sum(splines == x) == 1)
          {
            x <-
              paste(x, '+ I(',x, '^2)' , collapse = '')
          }else
          {
            x <- x
          }
        }))
      })
    }else{
      model_formula <- flatten(flatten(models[i, 'models']))
    }
    
    #Add interactions to the models and drop for each variable in turn      
    if (models[i, 'analysis'] %in% 'interaction')
    {
      model_formula <- model_formula %>% map(function(.){ . <- c(., paste(.[-(1:2)],':', .[2]))}) %>% rep(2)
      model_formula <- model_formula[-(1:2)]
      model_data <-  c(flatten(flatten(models[i,'data'])),flatten(flatten(models[i,'data']))[-(1:2)])
      model_formula <- c(model_formula[1], lapply(2:length(model_formula), function(.){ model_formula[[.]] <- model_formula[[.]][-.]}))
    }else
    {
      model_data <-  flatten(flatten(models[i, 'data'])) 
    }
    
    #Fit glm models with all variable combinations for this analysis  
    model_fit <- model_formula %>% map(function(.)
      paste(paste(.[1], ' ~ '), paste(.[-1], collapse = ' + '), collapse = '+')) %>%
      map2(model_data, quietly(function(model, df)
        glm(as.formula(model), data = df, family = binomial('logit'), maxit = 10000))) %>%  transpose()
    #stores model warnings and flags issues
    model_warnings <- model_fit$warnings
    model_warnings <- model_warnings %>% map(function(.) if (length(.) != 0) . <- . else . <- NA)
    model_warnings <- ifelse(sum(map_dbl(model_warnings,is.na)) == length(model_warnings), NA, model_warnings)
    model_fit <- model_fit$result
    
    #Rename coefficicients of spline for legibility
    if (flatten(flatten(models[i, 'level'])) == 'spline')
    {
      model_fit <- model_fit %>% map(function(.){
        for (j in 1:length(splines)) {
          if (length(grep(splines[j], names(.$coefficients))) != 0)
          {
            names(.$coefficients)[grep(splines[j], names(.$coefficients))] <-
              sapply(1:length(grep(
                splines[j], names(.$coefficients)
              )), function(k) {
                if (k < 5) {
                  paste(splines[j], k, sep = '')
                }else
                {
                  paste(flatten(models[,'variable'])[[i]], ':', splines[j], k, sep = '')
                }
                
              })
          }
        }
        return(.)
      }) 
    }
    
    #Fit anova to find the LRT p-value
    if (models[i, 'analysis'] %in% 'univariable')
    {
      model_random <-  flatten(flatten(models[i, 'data'])) %>%
        map(function(df, outcome = models[i, 'outcome'][[1]]) glm(as.formula(paste(outcome, '~ 1')), data = df, family = binomial('logit'), maxit = 10000, model=FALSE, y=FALSE))
      model_anova <- model_fit %>%  map2(model_random, function(f, r) anova(r, f, test = 'LRT')$'Pr(>Chi)'[2])
    }else
    {
      model_anova <- model_fit[-1] %>% map(function(.) anova(., model_fit[[1]], test = 'LRT')$'Pr(>Chi)'[2])
    }

    #Reassing NA results of anova as 1
    model_anova <- model_anova %>% map(function(.) if (is.na(.)) { 
                                                      . <- 1 
                                                        }else {
                                                          . <- .}) %>% flatten()
    
    #Round P values and catagorise by significance level - with adjustment for multiple testing if required
    model_anova <- model_anova %>% map(function(.) {
      if (BF_cor) {
        . <- p.adjust(., n = BF_scale , method = 'bonferroni')
      }
      
      p.value <- ifelse(. < 0.001, '<0.001',as.character(pretty_round(.,digits = 3)))
      })
    
    #Calculate the odds ratios with confidence intervals; drop the intercept
    model_OR <- model_fit %>% map(function(.) exp(cbind(OR = coef(.), confint.default(.)))) %>% 
      map(function(.) {ifelse(. > 10000,Inf,.)})  %>%
      map( function(.) pretty_round(., digits =  2)) %>% 
      map(function(.) paste(.[,1], ' (', .[,2], ' to ', .[,3], ')', sep = '')) %>%
      map(function(.) . <- .[-1]) 
    
    #Tidy glm data - for summary statistics
    model_summary <- model_fit %>%  map(function(.) tidy(.)[-1,]) %>% 
      map2(model_OR, function(x,OR) cbind(Variables = x[,1], OR, p.value = map_chr(x[,ncol(x)],function(.) ifelse(. < 0.001, '<0.001',as.character(pretty_round(.,digits = 3))))))
    
    #Delete unneccessary GLM models
    if (models[i, 'analysis'] %in% c('interaction'))
    {
      model_fit <- list(model_fit[[1]])
    }
    return(list(model_summary, model_fit, model_anova, model_warnings))})
  
  #Reallocate storage
  temp <- transpose(temp)
  model_summary <- temp[[1]]
  model_fit <- temp[[2]]
  model_anova <- temp[[3]]
  model_warnings <- temp[[4]]
  #bind into data frame   
  models <- models %>% mutate(glm.summary = model_summary ,glm = model_fit, anova = model_anova, warnings = model_warnings)
  return(models)
}
