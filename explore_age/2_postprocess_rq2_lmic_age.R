# join the runs and link to parameters
scenarios <- read_csv("scenarios_rq2_lmic_age.csv", show_col_types = FALSE)
df <- list.files(path = "raw_outputs/output_rq2_lmic_age/", pattern = ".rds")
df <- map(paste0("raw_outputs/output_rq2_lmic_age/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario")  %>%
  mutate(vaccine_doses = if_else(max_coverage == 0, 0, vaccine_doses)) %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(0,2,3), labels = c("Counterfactual", "2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(1,5,9), labels = c("Counterfactual", "60+ years vaccinated before booster introduced", "40+ years vaccinated before booster introduced"))) %>%
  mutate(rollout_rate = if_else(vacc_per_week == 0.015, "Default", if_else(vacc_per_week == 0.01, "Slower rollout", "None"))) %>%
  mutate(waning = if_else(period_s == 250 & t_period_l == 365, "Default", if_else(period_s == 150 & t_period_l == 200, "Slower waning", "None")))