

# Tables of Demographics --------------------------------------------------

#demographic summary
demographic_summary = function(var, var_words, data, PerDigits = 0)
{
    #construct data summary
    data_summary <- c('All', var) %>% map(function(.){
      if (. == 'All')
      {
        temp <- c('Total, all cases', nrow(data))
      }else{
        if (sum(class(unlist(data[,.])) %in% 'factor') == 1)
        {
          #Add main variable name
          temp <- c(c(var_words)[.], sum(!is.na(data[,.])))
          
          #percentage calc
          temp[2] <- paste(temp[2], ' (', pretty_round(as.numeric(temp[2])/nrow(data[,.])*100, digits = PerDigits), ')', collapse = '', sep = '')
          
          #Add each catagorical variable
          for (i in 1:length(levels(unlist(data[,.]))))
          {
            temp <- rbind(temp, c(paste('&nbsp;&nbsp; ',levels(unlist(data[,.]))[i]), 
                                  length(unlist(data[unlist(data[,.]) %in% levels(unlist(data[,.]))[i], .]))))
            
            
            temp[i + 1,2] <- paste('&nbsp;&nbsp; ', temp[i + 1,2], ' [*', pretty_round(as.numeric(temp[i + 1,2])/sum(!is.na(data[,.]))*100, digits = PerDigits), '*]', collapse = '', sep = '')
          }
        }else{
          #Account for non catagorical variables
          temp <- c(c(var_words)[.], sum(!is.na(data[,.])))
          
          temp[2] <- paste(temp[2], ' (', pretty_round(as.numeric(temp[2])/nrow(data[,.])*100, digits = PerDigits), ')', collapse = '', sep  = '')
          if (. != 'year')
          {
            #mean
            var <- as.numeric(unlist(na.omit(data[,.])))
            temp <- rbind(temp, c('&nbsp;&nbsp; Mean **[SD]**', paste('&nbsp;&nbsp; ', pretty_round(mean(var), digits = PerDigits), ' **[', pretty_round(sd(var),digits = PerDigits), ']**', collapse = '', sep = '')))
            #median
            temp <- rbind(temp, c('&nbsp;&nbsp; Median **[25%, 75%]**', paste('&nbsp;&nbsp; ',  pretty_round(median(var), digits = PerDigits), ' **[',paste(pretty_round(quantile(var)[c(2,4)],digits = PerDigits), collapse = ', '), ']**', collapse = '', sep = '')))
          }
         
        }
      }
      return(temp) })
    
    #Bind and transform data 
    data_summary <- do.call(rbind, data_summary) %>% as_data_frame()
    #Add meaningful column names
    colnames(data_summary) <- c('Demographic Characteristics', 'Total')
    
    return(data_summary)
}

#demographic summary
variable_summary = function(categories, var, var_words, df, PerDigits = 0)
{
  #construct data summary
  temp <- names(categories) %>% map(function(variable){
    temp <- categories[[variable]] %>% map(function(category)
      {
      if (!is.na(category) && category == '') category <- levels(df[[variable]])
      
      
      temp <- c('All', var) %>% map(function(.){
        if (. == 'All')
        {
          temp <- sum(df[[variable]] %in% category)
          
          #percentage calc
          temp <- paste(temp, ' {', pretty_round(as.numeric(temp)/nrow(df)*100, digits = PerDigits), '}', collapse = '', sep = '')
          
        }else{
          if (sum(class(df[[.]]) %in% 'factor') == 1)
          {
            #Add main variable name
            temp <- nrow(df[df[[variable]] %in% category & !is.na(df[[.]]),.])
            
            #percentage calc
            temp <- paste(temp, ' (', pretty_round(as.numeric(temp)/sum(df[[variable]] %in% category)*100, digits = PerDigits), ')', collapse = '', sep = '')
            
            #Add each catagorical variable
            for (i in 1:length(levels(df[[.]])))
            {
              temp <- rbind(temp, nrow(df[(df[[.]] %in% levels(df[[.]])[i] & df[[variable]] %in% category),]))
              
              
              temp[i + 1] <- paste('&nbsp;&nbsp; ', temp[i + 1], ' [*', pretty_round(as.numeric(temp[i + 1]) / nrow(df[df[[variable]] %in% category & !is.na(df[[.]]),.])*100, digits = PerDigits), '*]', collapse = '', sep = '')
            }
          }else{
            #Account for non catagorical variables
            temp <- nrow(df[df[[variable]] %in% category & !is.na(df[[.]]),.])
            
            temp <- paste(temp, ' (', pretty_round(as.numeric(temp)/sum(df[[variable]] %in% category)*100, digits = PerDigits), ')', collapse = '', sep = '')
            
            if (. != 'year')
            {
              #mean
              var <- as.numeric(unlist(df[df[[variable]] %in% category & !is.na(df[[.]]),.]))
              temp <- rbind(temp, paste('&nbsp;&nbsp; ', pretty_round(mean(var), digits = PerDigits), ' **[',pretty_round(sd(var),digits = PerDigits), ']**', collapse = '', sep = ''))
              #median
              temp <- rbind(temp, paste('&nbsp;&nbsp; ', pretty_round(median(var), digits = PerDigits), ' **[',paste(pretty_round(quantile(var)[c(2,4)],digits = PerDigits), collapse = ', '), ']**', collapse = '', sep = ''))
            }
          }
        }
        return(temp) })
      
      #Bind and transform data 
      temp <- do.call(rbind, temp) %>% as_data_frame()
    }) %>% bind_cols
    
    #name columns
    colnames(temp) <- names(categories[[variable]])
    return(temp)
  }) %>% bind_cols
  
  return(temp)
}