name <- "rq1_hic_abmodel_age"

# join the runs and link to parameters
scenarios <- read_csv(paste0("scenarios_", name, ".csv"), show_col_types = FALSE)
df <- list.files(path = paste0("raw_outputs/output_", name, "/"), pattern = ".rds")
df <- map(paste0("raw_outputs/output_", name, "/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") 

# name the options
df <- df %>%
  mutate(strategy_name = 
           if_else(vaccine_doses == 2 & age_groups_covered == 15, "10y+ 2 doses",
                   if_else(vaccine_doses == 3 & age_groups_covered == 15 & age_groups_covered_d3 == 5, "10y+ 2 doses, booster 60y+",
                           if_else(vaccine_doses == 3 & age_groups_covered == 15 & age_groups_covered_d3 == 9, "10y+ 2 doses, booster 40y+",
                                   if_else(vaccine_doses == 3 & age_groups_covered == 15 & age_groups_covered_d3 == 13, "10y+ 2 doses, booster 20y+",
                                           if_else(vaccine_doses == 3 & age_groups_covered == 15 & age_groups_covered_d3 == 15, "10y+ 2 doses, booster 10y+", "NA")))))) %>%
  mutate(rollout_rate = if_else(vacc_per_week == 0.05, "Default", if_else(vacc_per_week < 0.05, "Slower rollout", "None"))) %>%
  mutate(dose_3_timing = if_else(t_d3 == 180, "6 months (default)",
                                 if_else(t_d3 == 240, "8 months", 
                                         if_else(t_d3 == 360, "12 months", "NA"))))

m <- unique(df$strategy_name)
m
 df <- df %>%
   mutate(strategy_name = factor(strategy_name, levels = c(m[1], m[2], m[3], m[4], m[5]), ordered = TRUE))

# summarise totals over repetitions
summary_df <- df %>%
  filter(compartment %in% c("D")) %>%
  filter(timestep == max(df$timestep) | timestep == (365-31)) %>%
  pivot_wider(names_from = timestep, values_from = value) %>%
  mutate(value = `1246` - `334`) %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, strategy_name, ab_model_infection, vacc_per_week, t_d3, dose_3_timing, rollout_rate, repetition, compartment, age) %>%
  summarise(value_med = median(value),
         value_lower = quantile(value, 0.025),
         value_upper = quantile(value, 0.975)) %>%
  ungroup() %>%
  mutate(age = as.numeric(age))

saveRDS(summary_df, paste0("processed_outputs/df_summarise_", name, ".rds"))

