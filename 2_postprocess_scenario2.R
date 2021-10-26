# join the runs and link to parameters
scenarios <- read_csv("scenarios_scenario2.csv", show_col_types = FALSE)
df <- list.files(path = "output_cluster_scenario2/", pattern = ".rds")
df <- map(paste0("output_cluster_scenario2/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") %>%
  mutate(vaccine_doses = if_else(max_coverage == 0, 0, vaccine_doses)) %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(0,2,3), labels = c("Counterfactual", "2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(1,5,9), labels = c("Counterfactual", "60+ years", "40+ years")))

# summarise totals over repetitions
df <- df %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase) %>%
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
  group_by(timestep, income_group, target_pop, max_coverage, age_groups_covered, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, deaths_med, deaths_lower, deaths_upper, prop_R_med, total_doses_med) %>%
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

saveRDS(df_summarise, "processed_outputs/df_summarise_scenario2.rds")
saveRDS(df_summarise_totals, "processed_outputs/df_summarise_totals_scenario2.rds")

# counterfactual over time
g1 <- ggplot(data = filter(df_summarise, max_coverage == 0), aes(x = as.Date(date), y = deaths_t/target_pop * 1e6)) +
  geom_line() +
  facet_wrap(~income_group) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "date", y = "deaths per day per million", ) +
  ggtitle("Counterfactual epidemic")
g1

# show total doses over time as a check
df2 <- filter(df_summarise, max_coverage == 0.8) 
g2 <- ggplot(data = df2, aes(x = as.Date(date), y  = vaccines_t)) +
  geom_line() +
  facet_wrap( income_group ~age_groups_covered + vaccine_doses , nrow = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "date", y = "cumulative vaccines delivered", ) +
  ggtitle("Vaccines delivered")
g2
ggsave("plots/scenario2_cumulative_doses.png", height = 12, width = 8)


# show recovered over time
df3 <- df_summarise %>%
  filter(max_coverage == 0)
g3 <- ggplot(data = df3, aes(x = as.Date(date), y = prop_R)) +
  facet_wrap(~income_group) +
  geom_line()+
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "date", y = "% in recovered class")+
  ggtitle("Proportion in Recovered")
g3

# show Rt over time
df4 <- df_summarise %>%
  filter(max_coverage == 0)
g4 <- ggplot(data = df4, aes(x = as.Date(date), y = Rt)) +
  facet_wrap(~income_group) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "date", y = "Rt") +
  ggtitle("Reproduction number")
g4

# composite plot
library(patchwork)
plots_combined <- g1 / g3 / g4
plots_combined

ggsave("plots/scenario2_setup.png", height = 12, width = 8)
