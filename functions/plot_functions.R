
#Produce forest plot summarising variable effect
forest_plot<- function(df, outcomes, outcomes_words, variables_words,level){
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  
  #Reorder outcomes and replace with words
  df$outcome <- factor(df$outcome, levels = rev(outcomes))
  df$Outcomes <- df$outcome
  
  #Rename variables
  df$Analysis <- df$analysis
  
  plot <- ggplot(df, aes(x=Outcomes, y=OR, ymin=LCI, ymax=UCI, fill=Analysis, colour=Analysis, shape=Analysis))+
    geom_pointrange(position = position_dodge(width = 0.2)) +
    geom_hline(yintercept=1, lty=2) +
    scale_x_discrete(labels = rev(outcomes_words)) +
    coord_flip() +
    theme_bw()
  
  return(plot)
}

#Produce plots of the models fitted against the data
outcome_variable_plot <- function(df, outcome_words, variables_words, k)
{
  plot <- ggplot(df, aes(x=age, y=fit, ymin=LCI, ymax=UCI, fill=type, colour=type, shape=type)) +
    geom_pointrange(position = "jitter") +
    ylab('proportion') + xlab('age') +
    ylim(0,1) +
    theme_bw() +
    facet_wrap(~level, ncol=2)
  
  return(plot)
}

#Produce forest plot summarising variable effect - including stepwise
forest_plot_stepwise<- function(df, outcomes, outcomes_words, variables_words,level,stepwise){
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  
  #correct stepwise formating
  stepwise <- stepwise %>% mutate(OR=as.numeric(OR), LCI=as.numeric(LCI), UCI=as.numeric(UCI),analysis=rep('stepwise', nrow(stepwise)))
  #Filter dataframe
  df <- df[, c('variable', 'outcome', 'OR', 'LCI', 'UCI', 'analysis')]
  
  #bind data frame together
  df <- bind_rows(df,stepwise)
  
  #Reorder outcomes and replace with words
  df$outcome <- factor(df$outcome, levels = rev(outcomes))
  df$Outcomes <- df$outcome
  
  #Rename 
  df$Analysis <- df$analysis
  
  plot <- ggplot(df, aes(x=Outcomes, y=OR, ymin=LCI, ymax=UCI, fill=Analysis, colour=Analysis, shape=Analysis))+
    geom_pointrange(position = position_dodge(width = 0.2)) +
    geom_hline(yintercept=1, lty=2) +
    scale_x_discrete(labels = rev(outcomes_words)) +
    coord_flip() +
    theme_bw()
  
  return(plot)
}

#Produce forest plot summarising variable effect
forest_plot_sensitivity <- function(df, outcomes, outcomes_words, variables_words,level){
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  
  #clean variable with no data
  df$OR <- lapply(1:nrow(df), function(i){ifelse(df$UCI[i] == Inf & df$LCI[i] == 0, NA, df$OR[i])}) %>% unlist
  df$LCI <- lapply(1:nrow(df), function(i){ifelse(df$UCI[i] == Inf & df$LCI[i] == 0, NA, df$LCI[i])}) %>% unlist
  df$UCI <- lapply(1:nrow(df), function(i){ifelse(df$UCI[i] == Inf & df$LCI[i] == 0, NA, df$UCI[i])}) %>% unlist
  
  #Reorder outcomes and replace with words
  df$outcome <- factor(df$outcome, levels = rev(outcomes))
  df$Outcomes <- df$outcome
 
  #Rename variables
  df$Analysis <- df$analysis
  df$Confounder <- df$covariate
  plot <- ggplot(df, aes(x=Outcomes, y=OR, ymin=LCI, ymax=UCI, fill=Confounder, colour=Confounder))+
    geom_pointrange(position = position_dodge(width = 0.8)) +
    geom_hline(yintercept=1, lty=2) +
    scale_x_discrete(labels = rev(outcomes_words)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(~category, ncol=1)
  
  return(plot)
}

#Produce forest plot summarising variable effect for a catagorical exposure
forest_plot_categorical<- function(df, outcomes, outcomes_words, variables_words,level){
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  
  #clean variable with no data
  df$OR <- lapply(1:nrow(df), function(i){ifelse(df$UCI[i] == Inf & df$LCI[i] == 0, NA, df$OR[i])}) %>% unlist
  df$LCI <- lapply(1:nrow(df), function(i){ifelse(df$UCI[i] == Inf & df$LCI[i] == 0, NA, df$LCI[i])}) %>% unlist
  df$UCI <- lapply(1:nrow(df), function(i){ifelse(df$UCI[i] == Inf & df$LCI[i] == 0, NA, df$UCI[i])}) %>% unlist
  
  #Reorder outcomes and replace with words
  df$outcome <- factor(df$outcome, levels = rev(outcomes))
  df$Outcomes <- df$outcome
  
  #Rename
  df$Analysis <- df$analysis
  
  plot <- ggplot(df, aes(x=Outcomes, y=OR, ymin=LCI, ymax=UCI, fill=Analysis, colour=Analysis, shape=Analysis))+
    geom_pointrange(position = position_dodge(width = 0.2)) +
    geom_hline(yintercept=1, lty=2) +
    scale_x_discrete(labels = rev(outcomes_words)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(~category, ncol=1)
  
  return(plot)
}

#Plot summary of missingness in the data
ggplot_missing <- function(x){
  x_order <- x %>% map(function(.) sum(is.na(.))) %>% unlist %>% sort %>% rev %>% names
  
  x <- x %>% is.na 
  x[,x_order] %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / notifications")
}
