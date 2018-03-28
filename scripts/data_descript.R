stew(file=paste0('../data/', folder, '/descriptive_data.rda'), {
# Distributions by age ----------------------------------------------------

#Distribution for age with BCG vaccination
age_bcg_no <- quantile(uk_not$age[uk_not$bcgvacc %in% 'No'], c(1/4,1/2,3/4)) %>% as.numeric
age_bcg_yes <- quantile(uk_not$age[uk_not$bcgvacc %in% 'Yes'], c(1/4,1/2,3/4)) %>% as.numeric

#Distribution for vaccinated at birth
age_vcbirth_yes <- quantile(uk_not$age[uk_not$ageatvac %in% '< 1'], c(1/4,1/2,3/4), na.rm=TRUE) %>% as.numeric
age_vcbirth_no <- quantile(uk_not$age[!(uk_not$ageatvac %in% c('< 1', NA))], c(1/4,1/2,3/4), na.rm=TRUE) %>% as.numeric

#Distribution for age with BCG vaccination
age_ukborn_no <- quantile(uk_not$age[uk_not$ukborn %in% 'Non-UK Born'], c(1/4,1/2,3/4),na.rm=TRUE) %>% as.numeric
age_ukborn_yes <- quantile(uk_not$age[uk_not$ukborn %in% 'UK Born'], c(1/4,1/2,3/4), na.rm=TRUE) %>% as.numeric



# Test differences with age -----------------------------------------------

#Test differences between 15-65
df <- uk_not
df$temp <- ifelse (uk_not$age < 65 & uk_not$age > 15, 'Yes', 'No') %>% factor
df <- df[,c('bcgvacc', 'temp')] %>% na.omit
prop_tab <- improved_table(df$temp, df$bcgvacc)

age_15_65_prop <- paste0(prop_tab[2, 7:8], '%, ', prop_tab[2, 1:2],'/', prop_tab[3, 1:2])
age_15_65_test <- prop_tab[2, 9]

#Test differences between <15
df <- uk_not
df$temp <- ifelse (uk_not$age < 15, 'Yes', 'No') %>% factor
df <- df[,c('bcgvacc', 'temp')] %>% na.omit
prop_tab <- improved_table(df$temp, df$bcgvacc)

age_15_prop <- paste0(prop_tab[2, 7:8], '%, ', prop_tab[2, 1:2],'/', prop_tab[3, 1:2])
age_15_test <- prop_tab[2, 9]

#Test differences between >65
df <- uk_not
df$temp <- ifelse (uk_not$age > 65, 'Yes', 'No') %>% factor
df <- df[,c('bcgvacc', 'temp')] %>% na.omit
prop_tab <- improved_table(df$temp, df$bcgvacc)

age_65_prop <- paste0(prop_tab[2, 7:8], '%, ', prop_tab[2, 1:2],'/', prop_tab[3, 1:2])
age_65_test <- prop_tab[2, 9]

# Sex differences ---------------------------------------------------------

#Test differences between sex

df <- uk_not
df$temp <- df$sex
df <- df[,c('temp')] %>% na.omit
prop_tab <- table(df$temp)

temp_prop <- prop.test(as.vector(prop_tab), rep(sum(prop_tab), 2))

gender_prop <-paste0(round(temp_prop$estimate*100, digits=1) %>% as.numeric, '%, ', prop_tab,'/', sum(prop_tab))
gender_test <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))

#Test differences - BCG vaccinated and sex
df <- uk_not
df$temp <- df$sex
df <- df[,c('bcgvacc', 'temp')] %>% na.omit
prop_tab <- improved_table(df$temp, df$bcgvacc)

gender_prop_BCG <- rev(paste0(prop_tab[1:2, 4], '%, ', prop_tab[2, 1:2],'/', prop_tab[3, 1:2]))

#To verify
temp_prop <- prop.test(c(length(uk_not$sex[(uk_not$bcgvacc %in% 'Yes' & uk_not$sex %in% 'Male')]),length(uk_not$sex[(uk_not$bcgvacc %in% 'Yes' &  uk_not$sex %in% 'Female')])), c(sum(!is.na(uk_not$sex[(!is.na(uk_not$bcgvacc) & uk_not$sex %in% 'Male')])),sum(!is.na(uk_not$sex[(!is.na(uk_not$bcgvacc) & uk_not$sex %in% 'Female')]))))

gender_test_BCG <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))

#Test differences - BCG vaccinated and vcbirth
df <- uk_not
df$temp <- ifelse (uk_not$ageatvac == '< 1', 'Yes', 'No') %>% factor
df <- df[,c('sex', 'temp')] %>% na.omit
prop_tab <- improved_table(df$temp, df$sex)

gender_prop_vcbirth <- rev(paste0(prop_tab[2, 7:8], '%, ', prop_tab[2, 1:2],'/', prop_tab[3, 1:2]))
gender_test_vcbirth <- prop_tab[2, 9]

# Test UK birth status ----------------------------------------------------

#Test differences between UK birth status
df <- uk_not
df$temp <- df$ukborn
df <- df[,c('temp')] %>% na.omit
prop_tab <- table(df$temp)

temp_prop <- prop.test(as.vector(prop_tab), rep(sum(prop_tab), 2))

ukborn_prop <- rev(paste0(round(temp_prop$estimate*100, digits=1) %>% as.numeric, '%, ', prop_tab,'/', sum(prop_tab)))
ukborn_test <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))

#Test differences - BCG vaccinated and Uk birth status
df <- uk_not
df$temp <- df$ukborn
df <- df[,c('bcgvacc', 'temp')] %>% na.omit
prop_tab <- improved_table(df$temp, df$bcgvacc)
improved_table(df$temp, df$bcgvacc)

ukborn_prop_BCG <- improved_table(df$temp, df$bcgvacc)

#To verify
temp_prop <- prop.test(c(length(uk_not$ukborn[(uk_not$bcgvacc %in% 'Yes' & uk_not$ukborn %in% 'UK Born')]),length(uk_not$ukborn[(uk_not$bcgvacc %in% 'Yes' &  uk_not$ukborn %in% 'Non-UK Born')])), c(sum(!is.na(uk_not$ukborn[(!is.na(uk_not$bcgvacc) & uk_not$ukborn %in% 'UK Born')])),sum(!is.na(uk_not$ukborn[(!is.na(uk_not$bcgvacc) & uk_not$ukborn %in% 'Non-UK Born')]))))

ukborn_test_BCG <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits = 3)))

#Test differences - Vaccinated last 10 years h and Uk birth status
df <- uk_not
df$temp <- ifelse(uk_not$yrsinceBCG == '\u2264 10', 'Yes', 'No') %>% factor
df <- df[,c('ukborn', 'temp')] %>% na.omit
prop_tab <- improved_table( df$ukborn, df$temp)

temp_prop <- prop.test(c(length(uk_not$ukborn[(uk_not$yrsinceBCG %in% '\u2264 10' & uk_not$ukborn %in% 'UK Born')]),length(uk_not$ukborn[(uk_not$yrsinceBCG %in% '\u2264 10' &  uk_not$ukborn %in% 'Non-UK Born')])), c(sum(!is.na(uk_not$ukborn[(!is.na(uk_not$yrsinceBCG) & uk_not$ukborn %in% 'UK Born')])),sum(!is.na(uk_not$ukborn[(!is.na(uk_not$yrsinceBCG) & uk_not$ukborn %in% 'Non-UK Born')]))))

ukborn_prop_vc10 <-rev(paste0(prop_tab[1:2, 4], '%, ', prop_tab[2, 1:2],'/', prop_tab[3, 1:2]))
ukborn_test_vc10 <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))

#Test differences - Vaccinated at birth and Uk birth status
df <- uk_not
df$temp <- ifelse (uk_not$ageatvac == '< 1', 'Yes', 'No') %>% factor
df <- df[,c('ukborn', 'temp')] %>% na.omit
prop_tab <- improved_table(df$temp, df$ukborn)

ukborn_prop_vcbirth <- rev(paste0(prop_tab[2, 7:8], '%, ', prop_tab[2, 1:2],'/', prop_tab[3, 1:2]))


temp_prop <- prop.test(c(length(uk_not$ukborn[(uk_not$ageatvac %in% '< 1' & uk_not$ukborn %in% 'UK Born')]),length(uk_not$ukborn[(uk_not$ageatvac %in% '< 1' &  uk_not$ukborn %in% 'Non-UK Born')])), c(sum(!is.na(uk_not$ukborn[(!is.na(uk_not$ageatvac) & uk_not$ukborn %in% 'UK Born')])),sum(!is.na(uk_not$ukborn[(!is.na(uk_not$ageatvac) & uk_not$ukborn %in% 'Non-UK Born')]))))

ukborn_test_vcbirth <- ifelse(temp_prop$p.value < 0.001, '< 0.001', as.character(round(temp_prop$p.value, digits=3)))


# Ethnic groups differences -----------------------------------------------

#Test Differences between ethnic groups
birth_stat <- list(c('UK Born', 'Non-UK Born'),c('UK Born'),c('Non-UK Born'))
ethgrp_prop <- birth_stat %>% map(function(i){
    df <- uk_not
    df$temp <- df$ukborn
    df <- df[,c('ethgrp', 'ukborn', 'bcgvacc', 'temp')] %>% na.omit
    df <- df[df$ukborn %in% i,]
    prop_tab <- improved_table(df$ethgrp, df$bcgvacc)
    
    prop <- cbind(paste0(prop_tab[-nrow(prop_tab), 7], '%, ', prop_tab[-nrow(prop_tab), 1],'/', prop_tab[nrow(prop_tab), 1]), paste0(prop_tab[-nrow(prop_tab), 8], '%, ', prop_tab[-nrow(prop_tab), 2],'/', prop_tab[nrow(prop_tab), 2]))
    test <- prop_tab[-nrow(prop_tab), 9]
    
    temp <- data_frame(Ethnicity=levels(df$ethgrp), Unvaccinated=prop[,1], Vaccinated=prop[,2], P=test, ord=as.numeric(prop_tab[-nrow(prop_tab), 7])) %>% arrange(ord) %>% select_(.dots=c('-ord'))
  return(temp)
})

#Test Differences between ethnic groups - vaccinated at birth
birth_stat <- list(c('UK Born', 'Non-UK Born'),c('UK Born'),c('Non-UK Born'))
ethgrp_prop_vcbirth <- birth_stat %>% map(function(i){
  df <- uk_not
  df$temp <- ifelse (uk_not$ageatvac == '< 1', 'Yes', 'No') %>% factor
  df <- df[,c('ethgrp', 'ukborn', 'temp')] %>% na.omit
  df <- df[df$ukborn %in% i,]
  prop_tab <- improved_table(df$ethgrp, df$temp)
  
  prop <- cbind(paste0(prop_tab[-nrow(prop_tab), 3], '%, ', prop_tab[-nrow(prop_tab), 1],'/', rowSums(matrix(as.numeric(prop_tab[-nrow(prop_tab), 1:2]), ncol=2))), paste0(prop_tab[-nrow(prop_tab), 4], '%, ', prop_tab[-nrow(prop_tab), 2],'/', rowSums(matrix(as.numeric(prop_tab[-nrow(prop_tab), 1:2]), ncol=2))))
  test <- prop_tab[-nrow(prop_tab), 5]
  
  temp <- data_frame(Ethnicity=levels(df$ethgrp), Notatbirth=prop[,1], Vacatbirth=prop[,2], P=test, ord=as.numeric(prop_tab[-nrow(prop_tab), 8])) %>% arrange(ord) %>% select_(.dots=c('-ord'))
  return(temp)
})


#Test differences between non-uk born -vaccinated at birth 
# Stratify by PHE ---------------------------------------------------------

#phec stratified by BCG vaccination
df <- uk_not
df$temp <- df$phec
df <- df[,c('bcgvacc', 'temp')] %>% na.omit

phec_bcg_tab <- improved_table(df$temp, df$bcgvacc)

rm("df")
})
