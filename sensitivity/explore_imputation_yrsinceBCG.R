## Load and extract imputed data, bind with original data to get age groups
imp_df_long_exp  <- complete(imp_df, action = 'long', include = FALSE) %>% 
  as_tibble

imp_df_long_exp <- imp_df_long_exp %>% 
  left_join(uk_not %>% 
              rowid_to_column() %>% 
              mutate(.id = as.factor(rowid)) %>% 
              select(.id, agegrp), 
            by = ".id")

## Exploratory functions
## summarise yrsinceBCG as percentage by group
yrsinceBCG_prop_by_group <- function(df, group = c("agegrp", "ukborn")) {
  dist_yrsinceBCG <- df %>% 
    filter(!(bcgvacc %in% c("No"))) %>% 
    dplyr::group_by(.dots = c(group, "yrsinceBCG")) %>% 
    dplyr::tally() %>% 
    group_by(.dots = group) %>% 
    dplyr::mutate(sum_yrsinceBCG = sum(n),
                  yrsinceBCG_prop = n / sum_yrsinceBCG) %>%
    select(-n)
  
  return(dist_yrsinceBCG)
}

## Average over imputed data sets and use above function
yrsinceBCG_prop_by_group_imp <- function(df, group = c("agegrp", "ukborn", ".imp")) {
  
  df %>% 
    filter(!is.na(ageatvac)) %>% 
    yrsinceBCG_prop_by_group(group = group) %>% 
    group_by(.dots = c(group, "yrsinceBCG")) %>% 
    mutate(yrsinceBCG_prop = mean(yrsinceBCG_prop, na.rm = T)) 
  
}

## Plot proportions
plot_yrsincebcg_prop <- function(df, facet_by_ageatvac = FALSE) {
  p <- df %>% 
    ggplot(aes(x = ukborn, y  = yrsinceBCG_prop, fill = yrsinceBCG)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal()
  
  if (facet_by_ageatvac) {
    p + facet_grid(agegrp ~ ageatvac)
  }else{
    p +    facet_wrap(~agegrp)
  }
}

## Summarise bcg vaccination status, by yrsinceBCG
## Original data set 
dist_yrsinceBCG <- uk_not %>% 
  yrsinceBCG_prop_by_group(group = c("agegrp", "ukborn"))

dist_yrsinceBCG %>% 
  plot_yrsincebcg_prop(facet_by_ageatvac = FALSE)
ggplot2::ggsave("explore_imp.pdf", path = "output/drafts/figures", device = "pdf")


## imputed
dist_yrsinceBCG_imp <- imp_df_long_exp %>%
  yrsinceBCG_prop_by_group_imp(group = c("agegrp", "ukborn", ".imp"))

dist_yrsinceBCG_imp %>% 
  plot_yrsincebcg_prop(facet_by_ageatvac = FALSE)
ggsave("explore_imp_imp.pdf", path = "output/drafts/figures/", device = "pdf")


## Summarise bcg vaccination status, by yrsinceBCG - strat by age at vaccination
## Original data
dist_yrsinceBCG_ageatvac <- uk_not %>% 
  yrsinceBCG_prop_by_group(group = c("agegrp", "ukborn", "ageatvac"))

dist_yrsinceBCG_ageatvac %>% 
  plot_yrsincebcg_prop(facet_by_ageatvac = TRUE)
ggsave("explore_imp_ageatvac.pdf", path =  "output/drafts/figures/", device = "pdf")


## Imputed
dist_yrsinceBCG_ageatvac_imp <- imp_df_long_exp %>%
  yrsinceBCG_prop_by_group_imp(group = c("agegrp", "ukborn", ".imp", "ageatvac"))


dist_yrsinceBCG_ageatvac_imp %>% 
  plot_yrsincebcg_prop(facet_by_ageatvac = TRUE)
ggsave("explore_imp_ageatvac_imp.pdf", path =  "output/drafts/figures/", device = "pdf")
