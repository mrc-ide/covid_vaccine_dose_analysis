name <- "rq3_hic_abmodel_zerocovid"

# join the runs and link to parameters
scenarios <- read_csv(paste0("scenarios_", name, ".csv"), show_col_types = FALSE)
df <- list.files(path = paste0("raw_outputs/output_", name, "/"), pattern = ".rds")
df <- map(paste0("raw_outputs/output_", name, "/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") 

# name the options
df <- df %>%
  mutate(strategy_name = 
           if_else(vaccine_doses == 2 & age_groups_covered == 15, "10y+ 2 doses, no booster",
                   if_else(vaccine_doses == 3 & age_groups_covered == 15 & age_groups_covered_d3 == 5, "10y+ 2 doses, booster 60y+",
                           if_else(vaccine_doses == 3 & age_groups_covered == 15 & age_groups_covered_d3 == 9, "10y+ 2 doses, booster 40y+",
                                   if_else(vaccine_doses == 3 & age_groups_covered == 15 & age_groups_covered_d3 == 13, "10y+ 2 doses, booster 20y+",
                                           if_else(vaccine_doses == 3 & age_groups_covered == 15 & age_groups_covered_d3 == 15, "10y+ 2 doses, booster 10y+", "NA")))))) %>%
  mutate(rollout_rate = if_else(vacc_per_week == 0.05, "Default", if_else(vacc_per_week < 0.05, "Slower rollout", "None"))) %>%
  mutate(dose_3_timing = if_else(t_d3 == 180, "6 months (default)",
                                 if_else(t_d3 == 240, "8 months", 
                                         if_else(t_d3 == 360, "12 months", "NA")))) %>% mutate(Rt_lift_t = if_else(R0_t3_in == "8/1/2021", "Sept '21 lift", if_else(R0_t3_in == "10/1/2021", "Nov '21 lift", if_else(R0_t3_in == "2/1/2022", "Mar '22 lift", "None"))))

m <- unique(df$strategy_name)
m
# df <- df %>%
#   mutate(strategy_name = factor(strategy_name, levels = c(m[1], m[4], m[5], m[2], m[3]), ordered = TRUE))

# summarise totals over repetitions
df <- df %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, strategy_name, ab_model_infection, vacc_per_week,  t_d3, waning, dose_3_timing, rollout_rate, Rt_lift_t) %>%
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
  group_by(timestep, income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, deaths_med, deaths_lower, deaths_upper, inc_med, inc_lower, inc_upper, prop_R_med, total_doses_med, strategy_name, vacc_per_week,  t_d3, waning, dose_3_timing, rollout_rate, Rt_lift_t) %>%
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
  filter(t_d3 == 180,
         vacc_per_week == 0.05) %>%
  filter(date < as.Date("2021-01-01")) %>%
  filter(strategy_name == "10y+ 2 doses") %>%
  mutate(strategy_name = "Pre-vaccine introduction",
         dose_3_timing = "Pre-vaccine introduction")

df0_post_vacc <- df_summarise %>%
  filter(date >= as.Date("2021-01-01"))

df_summarise <- rbind(df0_pre_vacc, df0_post_vacc)

saveRDS(df_summarise, paste0("processed_outputs/df_summarise_", name, ".rds"))
saveRDS(df_summarise_totals, paste0("processed_outputs/df_summarise_totals_", name, ".rds"))
