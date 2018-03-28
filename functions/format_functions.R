
# Paper fig and table ref -------------------------------------------------
figRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, prefix = options("figcap.prefix"), 
           sep = options("figcap.sep"), prefix.highlight = options("figcap.prefix.highlight")) {
    i <- which(names(tag) == label)
    if (length(i) == 0) {
      i <- length(tag) + 1
      tag <<- c(tag, i)
      names(tag)[length(tag)] <<- label
      used <<- c(used, FALSE)
      names(used)[length(used)] <<- label
      created <<- c(created, FALSE)
      names(created)[length(created)] <<- label
    }
    if (!missing(caption)) {
      created[label] <<- TRUE
      paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight, 
             " ", caption)
    } else {
      used[label] <<- TRUE
      paste(prefix, tag[label])
    }
  }
})

tabRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, prefix = options("tabcap.prefix"), 
           sep = options("tabcap.sep"), prefix.highlight = options("tabcap.prefix.highlight"), InSentance = FALSE) {
    i <- which(names(tag) == label)
    if (length(i) == 0) {
      i <- length(tag) + 1
      tag <<- c(tag, i)
      names(tag)[length(tag)] <<- label
      used <<- c(used, FALSE)
      names(used)[length(used)] <<- label
      created <<- c(created, FALSE)
      names(created)[length(created)] <<- label
    }
    if (!missing(caption)) {
      created[label] <<- TRUE
      paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight, 
             " ", caption)
    } else {
      used[label] <<- TRUE
      if (!InSentance) {
        prefix <- tolower(prefix)
      }
      paste(prefix, tag[label])
    }
  }
})

# Supp Info fig and tab ref -----------------------------------------------

SupfigRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, prefix = options("figcap.prefix"), 
           sep = options("figcap.sep"), prefix.highlight = options("figcap.prefix.highlight")) {
    i <- which(names(tag) == label)
    if (length(i) == 0) {
      i <- length(tag) + 1
      tag <<- c(tag, i)
      names(tag)[length(tag)] <<- label
      used <<- c(used, FALSE)
      names(used)[length(used)] <<- label
      created <<- c(created, FALSE)
      names(created)[length(created)] <<- label
    }
    if (!missing(caption)) {
      created[label] <<- TRUE
      paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight, 
             " ", caption)
    } else {
      used[label] <<- TRUE
      paste(prefix, tag[label])
    }
  }
})


SuptabRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, prefix = options("suptabcap.prefix"), 
           sep = options("suptabcap.sep"), prefix.highlight = options("suptabcap.prefix.highlight"),
           delin = options('suptab.delin'), InSentance = FALSE) {
    i <- which(names(tag) == label)
    if (length(i) == 0) {
      i <- length(tag) + 1
      tag <<- c(tag, i)
      names(tag)[length(tag)] <<- label
      used <<- c(used, FALSE)
      names(used)[length(used)] <<- label
      created <<- c(created, FALSE)
      names(created)[length(created)] <<- label
    }
    if (!missing(caption)) {
      created[label] <<- TRUE
      paste0(prefix.highlight, prefix, " ", delin, i, sep, prefix.highlight, 
             " ", caption)
    } else {
      used[label] <<- TRUE
      if (!InSentance) {
        prefix <- tolower(prefix)
      }
      paste0(prefix, " ", delin, tag[label])
    }
  }
})

## Pretty number rounding for output presentation 
pretty_round = function(x, digits = 2) {
  x <- x %>% 
        round(digits) %>% 
          format(nsmall = digits)
 x[] <- x %>% 
          str_trim(side = 'both')
            
  return(x)
}

## Inf replacement for large ORs

OR_inf_rep = function(OR, MaxSize = 100000) {
  OR <- OR %>% as.numeric 
  OR <- ifelse(OR > MaxSize, Inf, OR)
  return(OR)
}

## Pretty format for OR reporting

pretty_OR = function(OR, CINote = '95% CI ') {
   CINote <- paste0('(', CINote)
   OR <- OR %>% str_replace('\\(', CINote)
   
   return(OR)
}

##Pretty format after checking a result is smaller than a limit
pretty_round_inf = function(x, ...) {
  x <- x %>% OR_inf_rep 
  x <- ifelse(x == Inf, x, pretty_round(x))
  return(x)
}

## Pretty table output format
pretty_sum_results_table = function(df, col_names = NULL, footer = NULL, label = NULL, caption = NULL, cap_fun = tabRef, ...) {

  ## Add footer
  if (!is.null(footer)) {
    
    # get column name for footer location
    SaveColName <- colnames(df)[1]
    colnames(df)[1] <- 'FooterLoc'

    table <- df %>% 
      add_row(FooterLoc = footer) %>%
      mutate_all(.funs = funs(ifelse(is.na(.), '', .))) 
    
    colnames(table)[1] <- SaveColName
    
  }else {
    table <- df
  }
 
  ## Add new column names
  if (!is.null(col_names)) {
    colnames(table) <- col_names
  }
 

  ## Add dummy arguements for label and caption if not supplied
  if (is.null(label) && is.null(caption)) {
    cap <- ''
  }else {
    if (is.null(label)) {
      label = ''
    }
    
    if (is.null(caption)) {
      caption = ''
    }
    cap <- cap_fun(label, caption)
  }
  
  
  ## Add label and caption and make table
  kable(table, caption = cap, padding = 0, ...)
}

## Summarise sample size

sum_sample_size <- function(Var, VarAdj, PerDescrip = '% of all cases', Participants = participants_table) {
  ## Summarise sample sizes for footer
  SumSampleSize <-  Participants %>% 
    select_(.dots = c(Var, VarAdj))
  
  SumSampleSize <- paste0('* Univariable sample size for outcomes ordered as in table (', PerDescrip, ') = ', paste(SumSampleSize[[1]], collapse = ', '), ', \u2020 Multivariable sample size with outcomes ordered as in table (', PerDescrip,') = ', paste(SumSampleSize[[2]], collapse = ', '))
  
  return(SumSampleSize)
}

## Summarise results with data and model summaries
combine_model_data_sum = function(ModelSum, DataSum) {
  ## Add sample size for each category 
  DataSum %>%  
    select(Uni_Cases, Uni_CasesWithOutcome, Multi_Cases, Multi_CasesWithOutcome) %>%
         mutate(Uni_CasesWithOutcome = paste0(Uni_CasesWithOutcome, ' (', pretty_round(Uni_CasesWithOutcome / Uni_Cases * 100, digits = 0), ')') %>% str_replace(c('NA', 'Nan'), '-'), 
                Multi_CasesWithOutcome = paste0(Multi_CasesWithOutcome, ' (', pretty_round(Multi_CasesWithOutcome/Multi_Cases*100, digits = 0), ')') %>%  str_replace(c('NA', 'Nan'), '-')) -> DataSum
  
  ModelSum <- cbind(ModelSum[,1:2], 
                    DataSum %>% select(Uni_Cases, Uni_CasesWithOutcome),
                    ModelSum[,3:4],
                    DataSum %>% select(Multi_Cases, Multi_CasesWithOutcome), 
                    ModelSum[,5:6]
  )
  return(ModelSum)
}

## Find results that have no outcomes and set model outcomes as empty
remove_spurious_results = function(ModelSumTable) {
  ModelSumTable
                              
    
    
    
    
    
}

## Munge effect size estimates into cleaner format
ORFormat = function(String, pref = " (", Sep = ' to ', Digits = 2 ) {
  String <- String %>% 
    as.numeric %>% 
    pretty_round(digits = Digits) 
  String <- paste0(String[1], pref , String[2], Sep, String[3], ')' )
  
  return(String)
}