# join the runs and link to parameters
scenarios <- read_csv("scenarios_scenario1.csv", show_col_types = FALSE)
df <- list.files(path = "output_cluster_scenario1/", pattern = ".rds")
df <- map(paste0("output_cluster_scenario1/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") 

# name the options
df <- df %>%
  filter(!(vaccine_doses == 2 & age_groups_covered_d3 == 5)) %>%
  mutate(strategy_name = if_else(vaccine_doses == 2 & max_coverage != 0, "15y+ 2 doses",
                                 if_else(vaccine_doses == 3 & age_groups_covered_d3 == 5, "15y+ 2 doses, booster 60y+", if_else(vaccine_doses == 3 & age_groups_covered_d3 == 14, "15y+ 2 doses, booster 15y+", if_else(max_coverage == 0, "Counterfactual", "NA")))))

m <- unique(df$strategy_name)
df <- df %>%
  mutate(strategy_name = factor(strategy_name, levels = c(m[4], m[1], m[3], m[2])), ordered = TRUE)

# summarise totals over repetitions
df <- df %>%
  group_by(income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, strategy_name, ab_model_infection) %>%
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
  group_by(timestep, income_group, target_pop, max_coverage, age_groups_covered, age_groups_covered_d3, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, deaths_med, deaths_lower, deaths_upper, prop_R_med, total_doses_med, strategy_name, ab_model_infection) %>%
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

saveRDS(df_summarise, "processed_outputs/df_summarise_scenario1.rds")
saveRDS(df_summarise_totals, "processed_outputs/df_summarise_totals_scenario1.rds")
saveRDS(filter(df_summarise, max_coverage == 0), "processed_outputs/df_counter_scenario1.rds")

###################################################################################
# plots
icg <- "HIC"

# counterfactual over time
g1 <- ggplot(data = filter(df_summarise, max_coverage == 0, income_group == icg), aes(x = as.Date(date), y = deaths_t/target_pop * 1e6)) +
  geom_line() +
  facet_wrap(~ ab_model_infection, labeller = label_both) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "date", y = "deaths per day per million", ) +
  ggtitle(paste0("Counterfactual epidemic: ", icg))
g1

ggsave(paste0("plots/scenario1_counterfactual_", icg, ".png"), height = 4, width = 10)


# show total doses over time as a check
df2 <- filter(df_summarise, max_coverage == 0.8) 
g2 <- ggplot(data = df2, aes(x = as.Date(date), y  = vaccines_t)) +
  geom_line() +
  facet_wrap(vaccine_doses + age_groups_covered_d3 ~ income_group, nrow = 3) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "date", y = "cumulative vaccines delivered", ) +
  ggtitle("Vaccines delivered")
g2

# show recovered over time
df3 <- df_summarise %>%
  filter(max_coverage == 0)
g3 <- ggplot(data = df3, aes(x = as.Date(date), y = prop_R)) +
  facet_wrap(ab_model_infection~income_group) +
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

ggsave("plots/scenario1_setup.png", height = 12, width = 8)
