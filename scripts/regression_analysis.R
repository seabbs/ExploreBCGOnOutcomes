# Run and Summarise results-----------------------------------------------
stew(file=paste0('../data/', folder, '/regression_analysis.rda'),{
#enable compilation
enableJIT(3)

output <- regression_analysis(outcomes, variables, covariates, splines, outcomes_words, variables_words, covariates_words, splines_words, uk_not, BF_cor = FALSE, BF_scale = NULL)


#Summary table of output per variable against a single outcome
table_var_out <- output[[1]] 

#Summary table of output per variable against all outcomes
table_var_outs <- output[[2]]

#Summary table of LRT for comparing age terms
table_age_LRT <- output[[3]] %>% transpose()

#Summary of model sample size
ModelDataSum <- output[[4]]

#switch off JIT
enableJIT(0)

#remove output
rm('output')
})