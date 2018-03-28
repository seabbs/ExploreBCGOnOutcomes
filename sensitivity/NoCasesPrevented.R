bake(file = paste0('../data/', folder, '/predicted_cases_prevented.rds'), {
  
  unvac_outcome_count <- uk_not %>% 
    filter(bcgvacc %in% "No") %>% 
    filter(year >= 2009) %>% 
    select(Mortality) %>%
    dplyr::count(Mortality) %>% 
    dplyr::rename(Mortality_c = n) %>% 
    dplyr::filter(Mortality %in% "Yes") %>% 
    select(Mortality = Mortality_c) %>% 
    gather(key = "outcome", value = "cases")
  
  AvgYearlyPredictImpact <- unvac_outcome_count %>% 
    full_join(aOR.prog, by = "outcome") %>% 
    gather(key = "measure", value = "effect", OR, LCI, UCI) %>% 
    mutate(prev_cases = cases * (1 - effect)) %>% 
    mutate(prev_cases_per_year = prev_cases / length(unique(uk_not$year))) %>% 
    select(outcome, measure, prev_cases_per_year) %>% 
    spread(key = "measure", value = "prev_cases_per_year") %>% 
    group_by(outcome) %>% 
    mutate(pretty_eff_year = ORFormat(c(OR, UCI, LCI), pref = " (95% CI ", Digits = 0))

  unvac_mort_per_year <- round(unvac_outcome_count$cases[[1]] / length(unique(uk_not$year)), 0)
  
  tmp <- list(AvgYearlyPredictImpact, unvac_mort_per_year) 
}) -> tmp

AvgYearlyPredictImpact <- tmp[[1]]
unvac_mort_per_year  <- tmp[[2]]