# join the runs and link to parameters
scenarios <- read_csv("scenarios_scenario5_sensitivity.csv", show_col_types = FALSE)
df <- list.files(path = "output_cluster_scenario5_sensitivity/", pattern = ".rds")
df <- map(paste0("output_cluster_scenario5_sensitivity/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") 

# name the options
df <- df %>%
  filter(!(vaccine_doses == 2 & age_groups_covered_d3 == 5)) %>%
  mutate(strategy_name = if_else(vaccine_doses == 2 & max_coverage != 0, "40y+ 2 doses",
                                 if_else(vaccine_doses == 3 & age_groups_covered_d3 == 5, "40y+ 2 doses, booster 60y+", if_else(vaccine_doses == 3 & age_groups_covered_d3 == 9, "40y+ 2 doses, booster 40y+", if_else(max_coverage == 0, "Counterfactual", "NA")))))%>%
  mutate(rollout_rate = if_else(vacc_per_week == 0.015, "Default", if_else(vacc_per_week == 0.008, "Slower rollout", "None"))) %>%
  mutate(waning = if_else(period_s == 250 & t_period_l == 365, "Default", if_else(period_s == 150 & t_period_l == 200, "Slower waning", "None")))

m <- unique(df$strategy_name)
df <- df %>%
  mutate(strategy_name = factor(strategy_name, levels = c(m[4], m[1], m[3], m[2])), ordered = TRUE)

# summarise totals over repetitions
df <- df %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, strategy_name, ab_model_infection, vacc_per_week, period_s, t_period_l, t_d3) %>%
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
  group_by(timestep, income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, deaths_med, deaths_lower, deaths_upper, prop_R_med, total_doses_med, strategy_name, vacc_per_week, period_s, t_period_l, t_d3) %>%
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

saveRDS(df_summarise, "processed_outputs/df_summarise_scenario5_sensitivity.rds")
saveRDS(df_summarise_totals, "processed_outputs/df_summarise_totals_scenario5_sensitivity.rds")
saveRDS(filter(df_summarise, max_coverage == 0), "processed_outputs/df_counter_scenario5_sensitivity.rds")
