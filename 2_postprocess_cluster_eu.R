# join the runs and link to parameters
scenarios <- read_csv("scenarios_eu.csv", show_col_types = FALSE)
df <- list.files(path = "output_cluster_eu/", pattern = ".rds")
df <- map(paste0("output_cluster_eu/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") 

# summarise totals over repetitions
df <- df %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, dose_strategy) %>%
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
  group_by(timestep, income_group, target_pop, max_coverage, age_groups_covered, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, dose_strategy, deaths_med, deaths_lower, deaths_upper, prop_R_med, total_doses_med) %>%
  summarise(deaths_t = median(D_count),
            deaths_tmin = quantile(D_count, 0.025),
            deaths_tmax = quantile(D_count, 0.975),
            vaccines_t = median(X1_count + X2_count * 2 + X3_count * 3),
            dose1_t = median(X1_count),
            dose2_t = median(X2_count),
            dose3_t = median(X3_count),
            prop_R = round(median(R_count)/target_pop * 100,2),
            .groups = 'drop') %>%
  unique() %>%
  mutate(date = timestep + as.Date("2020-02-01"))

saveRDS(df_summarise, "processed_outputs/df_summarise_eu.rds")
saveRDS(df_summarise_totals, "processed_outputs/df_summarise_totals_eu.rds")

df1 <- filter(df_summarise, max_coverage == 0.8, income_group == "HIC") 
ggplot(data = df1, aes(x = timestep, y  = vaccines_t, col = factor(vaccine_doses))) +
  geom_line() +
  facet_wrap(vaccine_doses~age_groups_covered + dose_strategy)
