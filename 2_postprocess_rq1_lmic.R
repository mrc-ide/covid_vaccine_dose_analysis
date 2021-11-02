# join the runs and link to parameters
scenarios <- read_csv("scenarios_rq1_lmic.csv", show_col_types = FALSE)
df <- list.files(path = "raw_outputs/output_rq1_lmic/", pattern = ".rds")
df <- map(paste0("raw_outputs/output_rq1_lmic/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") 

# name the options
df <- df %>%
  filter(!(vaccine_doses == 2 & age_groups_covered_d3 == 5)) %>%
  mutate(strategy_name = if_else(vaccine_doses == 2 & max_coverage != 0, "40y+ 2 doses",
                                 if_else(vaccine_doses == 3 & age_groups_covered_d3 == 5, "40y+ 2 doses, booster 60y+", if_else(vaccine_doses == 3 & age_groups_covered_d3 == 9, "40y+ 2 doses, booster 40y+", if_else(max_coverage == 0, "Counterfactual", "NA")))))%>%
  mutate(rollout_rate = if_else(vacc_per_week == 0.015, "Default", if_else(vacc_per_week == 0.008, "Slower rollout", "None"))) %>%
  mutate(waning = if_else(period_s == 250 & t_period_l == 365, "Default", if_else(period_s == 150 & t_period_l == 200, "Slower waning", "None"))) %>%
  mutate(dose_3_timing = if_else(t_d3 == 240, "8 months (default)",
                                 if_else(t_d3 == 180, "6 months",
                                         if_else(t_d3 == 360, "12 months", "NA"))))

m <- unique(df$strategy_name)
df <- df %>%
  mutate(strategy_name = factor(strategy_name, levels = c(m[1], m[3], m[2])), ordered = TRUE)

# summarise totals over repetitions
df <- df %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, strategy_name, ab_model_infection, vacc_per_week, period_s, t_period_l, t_d3, waning, dose_3_timing, rollout_rate) %>%
  mutate(deaths_med = median(deaths),
         deaths_lower = quantile(deaths, 0.025),
         deaths_upper = quantile(deaths, 0.975),
         prop_R_med = median(prop_R),
         total_doses_med = median(total_doses)) %>%
  ungroup() 

df_summarise_totals <- df %>%
  select(-deaths, -total_doses, -prop_R, -cols, -repetition, - scenario) %>%
  unique()

# summarise temporal dynamics over repetitions
df_summarise <- df %>%
  unnest(cols) %>%
  select(-c(deaths, prop_R)) %>%
  group_by(timestep, income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, deaths_med, deaths_lower, deaths_upper, prop_R_med, total_doses_med, strategy_name, vacc_per_week, period_s, t_period_l, t_d3, waning, dose_3_timing, rollout_rate) %>%
  summarise(deaths_t = median(D_count),
            deaths_tmin = quantile(D_count, 0.025),
            deaths_tmax = quantile(D_count, 0.975),
            vaccines_t = median(X1_count + X2_count * 2 + X3_count * 3),
            dose1_t = median(X1_count),
            dose2_t = median(X2_count),
            dose3_t = median(X3_count),
            prop_R = round(median(R_count)/target_pop * 100,2),
            Rt = median(Rt),
            .groups = 'drop') %>%
  unique() %>%
  mutate(date = timestep + as.Date("2020-02-01"))

# calculate deaths since booster start
df_summarise_since_boost <- df %>%
  unnest(cols) %>%
  select(-c(deaths, prop_R)) 
# get date that booster starts
time_boost_start <- df_summarise_since_boost %>%
  filter(X3_count > 0) %>%
  group_by(vacc_per_week,t_d3) %>%
  filter(timestep == min(timestep)) %>%
  select(vacc_per_week, t_d3, timestep) %>%
  rename("time_since_boost" = "timestep") %>%
  unique()
  
df_summarise_since_boost <- df_summarise_since_boost %>%
  left_join(time_boost_start) %>%
  filter(timestep >= time_since_boost) %>%
  # calculate deaths for each repetition
  group_by(repetition, income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, strategy_name, vacc_per_week, period_s, t_period_l, t_d3, waning, dose_3_timing, rollout_rate) %>%
  summarise(deaths = sum(D_count)) %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, strategy_name, vacc_per_week, period_s, t_period_l, t_d3, waning,dose_3_timing, rollout_rate) %>%
  summarise(deaths_since_boost_med = median(deaths),
         deaths_since_boost_lower = quantile(deaths, 0.025),
         deaths_since_boost_upper = quantile(deaths, 0.975)) %>%
  ungroup() 

df_summarise_totals <- left_join(df_summarise_totals, df_summarise_since_boost)

saveRDS(df_summarise, "processed_outputs/df_summarise_rq1_lmic.rds")
saveRDS(df_summarise_totals, "processed_outputs/df_summarise_totals_rq1_lmic.rds")
