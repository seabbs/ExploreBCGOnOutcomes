
power_calc <- function(data, outcome, variable, cov = '', outcomes_words)
{
  # Restrict data set -------------------------------------------------------
  #Compile model variables to select correct data
  if (cov[1] != c(''))
  {
    model <- c(variable, cov)
  }else{
    model <- variable
  }
  
  data <- data[,c(outcome, model)] %>% na.omit 
  
  #sample size
  n <- data %>% nrow
  
  #Outcome rate given no variable
  p_1 <- data[,variable] %>% flatten %>% map(function(.)ifelse(. == 1,1,0)) %>% unlist %>% as.logical
  p_1 <- data[p_1,outcome] %>% flatten %>% map(function(.)ifelse(. == 2,1,0)) %>% unlist
  p_1 <- sum(p_1)/length(p_1)
  
  #Outcome rate given variable
  p_2 <- data[,variable] %>% flatten %>% map(function(.)ifelse(. == 2,1,0)) %>% unlist %>% as.logical
  p_2 <- data[p_2,outcome] %>% flatten %>% map(function(.)ifelse(. == 2,1,0)) %>% unlist
  p_2 <- sum(p_2)/length(p_2)
  
  #Outcome rate in sample
  out_prop <- data[,variable] %>% flatten %>% map(function(.)ifelse(. == 2,1,0)) %>% unlist
  out_prop <- sum(out_prop)/length(out_prop)
  
  #Calulate power
  power <- powerLogisticBin(n, p_1, p_2, out_prop, alpha = 0.05)
  
  #pretty_round analysis
  p_1 <- pretty_round(p_1, digits = 3)
  p_2 <- pretty_round(p_2, digits = 3)
  out_prop <- pretty_round(out_prop, digits = 3)
  power <- pretty_round(power*100, digits = 0)
  
  out <- data_frame(Outcome = outcomes_words[outcome], analysis = ifelse(cov[1] == '', 'Univariate', 'Adjusted'), `Sample size`= n,
                    `Pr(Y|X=0)` = p_1, `Pr(Y|X=1)` = p_2, `Pr(X=1)` = out_prop, Power = power)
  return(out)
}




