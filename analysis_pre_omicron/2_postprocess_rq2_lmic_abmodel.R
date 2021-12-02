name <- "rq2_lmic_abmodel"

# join the runs and link to parameters
scenarios <- read_csv(paste0("scenarios_", name, ".csv"), show_col_types = FALSE)
df <- list.files(path = paste0("raw_outputs/output_", name, "/"), pattern = ".rds")
df <- map(paste0("raw_outputs/output_", name, "/", df), readRDS)
df <- do.call(rbind,df)

df <- left_join(df, scenarios, by = "scenario") %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2,3), labels = c("2 doses", "3 doses"))) %>%
  mutate(age_groups_covered_label = factor(age_groups_covered, levels = c(5,9), labels = c("60+ years vaccinated before booster introduced", "40+ years vaccinated before booster introduced"))) %>%
  mutate(rollout_rate = if_else(vacc_per_week == 0.02, "Default", if_else(vacc_per_week == 0.01, "Slower rollout", "None"))) %>%
  mutate(waning = if_else(period_s == 250 & t_period_l == 365, "Default", if_else(period_s == 150 & t_period_l == 200, "Slower vacc waning", "None")))

# summarise totals over repetitions
df <- df %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_label, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, ab_model_infection, waning, rollout_rate, vacc_per_week, period_s, t_period_l) %>%
  mutate(deaths_med = median(deaths),
         deaths_lower = quantile(deaths, 0.025),
         deaths_upper = quantile(deaths, 0.975),
         inc_med = median(inc),
         inc_lower = quantile(inc, 0.025),
         inc_upper = quantile(inc, 0.975),
         prop_R_med = median(prop_R),
         total_doses_med = median(total_doses)) %>%
  ungroup()  

df_summarise_totals <- df %>%
  select(-deaths, -inc, -total_doses, -prop_R, -cols, -repetition, - scenario) %>%
  unique()

# summarise temporal dynamics over repetitions
df_summarise <- df %>%
  unnest(cols) %>%
  select(-c(deaths, prop_R, inc)) %>%
  group_by(timestep, income_group, target_pop, max_coverage, age_groups_covered,age_groups_covered_label, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, ab_model_infection, waning, rollout_rate, vacc_per_week, period_s, t_period_l, deaths_med, deaths_lower, deaths_upper, inc_med, inc_lower, inc_upper, prop_R_med, total_doses_med) %>%
  summarise(deaths_t = median(D_count),
            deaths_tmin = quantile(D_count, 0.025),
            deaths_tmax = quantile(D_count, 0.975),
            inc_t = median(incidence),
            inc_tmin = quantile(incidence, 0.025),
            inc_tmax = quantile(incidence, 0.975),
            vaccines_t = median(X1_count + X2_count * 2 + X3_count * 3),
            dose1_t = median(X1_count),
            dose2_t = median(X2_count),
            dose3_t = median(X3_count),
            prop_R = round(median(R_count)/target_pop * 100,2),
            Rt = median(Rt),
            .groups = 'drop') %>%
  unique() %>%
  mutate(date = timestep + as.Date("2020-02-01"))

# tidy up period before vacc introduction
df0_pre_vacc <- df_summarise %>%
  filter(period_s == 250,
         t_period_l == 365,
         vacc_per_week == 0.02) %>%
  filter(date < as.Date("2021-01-01")) %>%
  filter(vaccine_doses == "2 doses") %>%
  mutate(vaccine_doses = "Pre-vaccine introduction")

df0_post_vacc <- df_summarise %>%
  filter(date >= as.Date("2021-01-01"))

df_summarise <- rbind(df0_pre_vacc, df0_post_vacc)

saveRDS(df_summarise, paste0("processed_outputs/df_summarise_", name, ".rds"))
saveRDS(df_summarise_totals, paste0("processed_outputs/df_summarise_totals_", name, ".rds"))
