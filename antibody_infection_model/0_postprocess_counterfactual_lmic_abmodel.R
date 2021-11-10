# join the runs and link to parameters
scenarios <- read_csv("antibody_infection_model/scenarios_counter_lmic_abmodel.csv", show_col_types = FALSE)
df <- list.files(path = "raw_outputs/output_counter_lmic_abmodel/", pattern = ".rds")
df <- map(paste0("raw_outputs/output_counter_lmic_abmodel/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") 

# summarise temporal dynamics over repetitions
df_summarise <- df %>%
  unnest(cols) %>%
  select(-c(deaths, prop_R)) %>%
  group_by(timestep, income_group, target_pop, max_coverage, age_groups_covered, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, vacc_per_week, period_s, t_period_l) %>%
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


saveRDS(df_summarise, "processed_outputs/ab_infection_model/df_summarise_counter_lmic_abmodel.rds")

p1 <- ggplot(df_summarise, aes(x = as.Date(date), y = deaths_t)) +
  geom_line() +
  theme_bw() +
  labs(x = "year", y = "daily deaths") +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0),
        legend.position = "none"
  )
  
p1

p2 <- ggplot(df_summarise, aes(x = as.Date(date), y = Rt)) +
  geom_line() +
  theme_bw() +
  labs(x = "year", y = "Rt") +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0),
        legend.position = "none"
  )
p2

# combine plots
library(patchwork)
combined <- p1 / p2 + plot_annotation(tag_levels = "A")+ plot_layout(guides = "collect")
combined

ggsave("plots/counterfactual_lmic_abmodel.png", height = 8, width = 8)
