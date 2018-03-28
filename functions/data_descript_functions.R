
#Distribution for age with BCG vaccination
age_bcg_no <- quantile(uk_not$age[uk_not$bcgvacc %in% 'No'], c(1/4,1/2,3/4)) %>% as.numeric
age_bcg_yes <- quantile(uk_not$age[uk_not$bcgvacc %in% 'Yes'], c(1/4,1/2,3/4)) %>% as.numeric

#Test differences between 15-65
temp_prop <- prop.test(c(length(uk_not$age[(uk_not$bcgvacc %in% 'No' & uk_not$age > 15 & uk_not$age < 65)]),length(uk_not$age[(uk_not$bcgvacc %in% 'Yes' & uk_not$age > 15 & uk_not$age < 65)])), c(length(uk_not$age[uk_not$bcgvacc %in% 'No']),length(uk_not$age[uk_not$bcgvacc %in% 'Yes'])))

age_15_65_prop <-round(temp_prop$estimate*100, digits=1) %>% as.numeric
age_15_65_test <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))

#Test differences between <15
temp_prop <- prop.test(c(length(uk_not$age[(uk_not$bcgvacc %in% 'No' & uk_not$age < 15)]),length(uk_not$age[(uk_not$bcgvacc %in% 'Yes' & uk_not$age < 15)])), c(length(uk_not$age[uk_not$bcgvacc %in% 'No']),length(uk_not$age[uk_not$bcgvacc %in% 'Yes'])))

age_15_prop <-round(temp_prop$estimate*100, digits=1) %>% as.numeric
age_15_test <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))

#Test differences between >65
temp_prop <- prop.test(c(length(uk_not$age[(uk_not$bcgvacc %in% 'No' & uk_not$age > 65)]),length(uk_not$age[(uk_not$bcgvacc %in% 'Yes' & uk_not$age > 65)])), c(length(uk_not$age[uk_not$bcgvacc %in% 'No']),length(uk_not$age[uk_not$bcgvacc %in% 'Yes'])))

age_65_prop <-round(temp_prop$estimate*100, digits=1) %>% as.numeric
age_65_test <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))

#Test differences between sex
temp_prop <- prop.test(c(length(uk_not$sex[(uk_not$sex %in% 'Female')]),length(uk_not$sex[( uk_not$sex %in% 'Male')])), c(sum(!is.na(uk_not$sex)),sum(!is.na(uk_not$age))))

gender_prop <-round(temp_prop$estimate*100, digits=1) %>% as.numeric
gender_test <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))

#Test differences - BCG vaccinated
temp_prop <- prop.test(c(length(uk_not$sex[(uk_not$bcgvacc %in% 'Yes' & uk_not$sex %in% 'Female')]),length(uk_not$sex[(uk_not$bcgvacc %in% 'Yes' &  uk_not$sex %in% 'Male')])), c(sum(!is.na(uk_not$sex[uk_not$bcgvacc %in% 'Yes'])),sum(!is.na(uk_not$age[uk_not$bcgvacc %in% 'Yes']))))

gender_prop_BCG <-round(temp_prop$estimate*100, digits=1) %>% as.numeric
gender_test_BCG <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))

#Test Differences between ethnic groups
birth_stat <- list(c('UK Born', 'Non-UK Born'),c('UK Born'),c('Non-UK Born'))
ethgrp_prop <- birth_stat %>% map(function(i){
  temp <- levels(uk_not$ethgrp) %>% map_df(function(.){
    #prop test with various restricitons
    temp_prop <- prop.test(c(length(uk_not$bcgvacc[(uk_not$bcgvacc %in% 'No' & uk_not$ethgrp %in% . & uk_not$ukborn %in% i)]),length(uk_not$bcgvacc[(uk_not$bcgvacc %in% 'Yes' & uk_not$ethgrp %in% . & uk_not$ukborn %in% i)])), c(length(uk_not$bcgvacc[(uk_not$bcgvacc %in% 'No' & uk_not$ukborn %in% i)]),length(uk_not$bcgvacc[(uk_not$bcgvacc %in% 'Yes' & uk_not$ukborn %in% i)])))
    
    #format results
    prop <-round(temp_prop$estimate*100, digits=1) %>% as.numeric
    test <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))
    return(data_frame(Ethnicity=., Unvaccinated=prop[1], Vaccinated=prop[2], P=test))
  }) %>% arrange(Unvaccinated)
  return(temp)
})



#Test differences between PHEC
phec_prop <- levels(uk_not$phec) %>% map_df(function(.){
  temp_prop <- prop.test(c(length(uk_not$age[(uk_not$bcgvacc %in% 'No' & uk_not$phec %in% .)]),length(uk_not$age[(uk_not$bcgvacc %in% 'Yes' & uk_not$phec %in% .)])), c(length(uk_not$age[uk_not$bcgvacc %in% 'No']),length(uk_not$age[uk_not$bcgvacc %in% 'Yes'])))
  
  prop <-round(temp_prop$estimate*100, digits=1) %>% as.numeric
  test <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))
  return(data_frame(Ethnicity=., Unvaccinated=prop[1], Vaccinated=prop[2], P=test))
}) %>% arrange(Unvaccinated)