# join the runs and link to parameters
scenarios <- read_csv("scenarios_rq2_lmic.csv", show_col_types = FALSE)
df <- list.files(path = "raw_outputs/output_rq2_lmic/", pattern = ".rds")
df <- map(paste0("raw_outputs/output_rq2_lmic/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") %>%
  mutate(vaccine_doses = if_else(max_coverage == 0, 0, vaccine_doses)) %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(0,2,3), labels = c("Counterfactual", "2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(1,5,9), labels = c("Counterfactual", "60+ years vaccinated before booster introduced", "40+ years vaccinated before booster introduced"))) %>%
  mutate(rollout_rate = if_else(vacc_per_week == 0.015, "Default", if_else(vacc_per_week == 0.008, "Slower rollout", "None"))) %>%
  mutate(waning = if_else(period_s == 250 & t_period_l == 365, "Default", if_else(period_s == 150 & t_period_l == 200, "Slower waning", "None")))

# summarise totals over repetitions
df <- df %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, ab_model_infection, waning, rollout_rate) %>%
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
  group_by(timestep, income_group, target_pop, max_coverage, age_groups_covered, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, ab_model_infection, waning, rollout_rate, deaths_med, deaths_lower, deaths_upper, prop_R_med, total_doses_med) %>%
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

saveRDS(df_summarise, "processed_outputs/df_summarise_rq2_lmic.rds")
saveRDS(df_summarise_totals, "processed_outputs/df_summarise_totals_rq2_lmic.rds")
