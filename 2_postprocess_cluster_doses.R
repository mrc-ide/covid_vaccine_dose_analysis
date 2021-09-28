col1 <- "#5da0b5"
col2 <- "#c59e96"
col3 <- "#747473"
col4 <- "#5c8e72"
col5 <- "#2a73bb"

# join the runs and link to parameters
scenarios <- read_csv("scenarios_cluster_v4.csv")
df <- list.files(path = "output_cluster_v4/", pattern = ".rds")
df <- map(paste0("output_cluster_v4/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") %>%
  filter(age_groups_covered %in% c(5,8))

# summarise over repetitions
df_summarise <- df %>%
  unnest(cols) %>%
  group_by(income_group, target_pop, R0, Rt1, Rt2, max_coverage, age_groups_covered, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase) %>%
mutate(deaths_med = median(deaths),
       deaths_lower = quantile(deaths, 0.025),
       deaths_upper = quantile(deaths, 0.975),
       prop_R_med = median(prop_R)) %>%
  ungroup() %>%
  select(-c(deaths, prop_R)) %>%
  group_by(timestep, income_group, target_pop, R0, Rt1, Rt2, max_coverage, age_groups_covered, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, deaths_med, deaths_lower, deaths_upper, prop_R_med) %>%
  summarise(deaths_t = median(D_count),
            deaths_tmin = quantile(D_count, 0.025),
            deaths_tmax = quantile(D_count, 0.975),
            vaccines_t = median(X1_count + X2_count + X3_count),
            dose1_t = mean(X1_count),
            dose2_t = mean(X2_count),
            dose3_t = mean(X3_count),
            prop_R = round(median(R_count)/target_pop * 100,2),
            .groups = 'drop') %>%
  unique()

counterfactual <- df_summarise %>%
  filter(max_coverage == 0) %>%
  select(timestep, income_group, R0, Rt1, Rt2, target_pop, deaths_t, deaths_tmin, deaths_tmax)

df1 <- filter(df_summarise, max_coverage == 0.8, income_group == "HIC") %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(5,8), labels = c("60+ years", "45+ years")))

# plot outputs, vaccine doses
df1_vacc <- df1 %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose")

ggplot(data = df1_vacc, aes(x = timestep, y = value/target_pop*100, col = dose)) +
  geom_line(size = 0.8) +
  facet_grid(as.factor(vaccine_doses) ~ age_groups_covered) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Timestep (days)", y = "Vaccinated (%)", col = "Dose number") +
  ggtitle("High income setting") +
  annotate(geom="text", x=500, y=25, xend=Inf, yend=Inf, label='DRAFT', color='grey', angle=45, fontface='bold', size=15, alpha=0.2, family='Arial')

ggsave("plots/Fig3_HIC_vaccinated.png", height = 5, width = 10)

####################################################################
df1 <- filter(df_summarise, max_coverage == 0.8, income_group == "LMIC") %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(5,8), labels = c("60+ years", "45+ years"))) %>%
  filter(age_groups_covered == "45+ years")

# plot outputs, vaccine doses
df1_vacc <- df1 %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose")

ggplot(data = df1_vacc, aes(x = timestep, y = value/target_pop*100, col = dose)) +
  geom_line(size = 0.8) +
  facet_grid(as.factor(vaccine_doses) ~ age_groups_covered) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Timestep (days)", y = "Vaccinated (%)", col = "Dose number") +
  ggtitle("Lower-middle income setting") +
  annotate(geom="text", x=500, y=15, xend=Inf, yend=Inf, label='DRAFT', color='grey', angle=45, fontface='bold', size=15, alpha=0.2, family='Arial')

ggsave("plots/Fig3_LMIC_vaccinated.png", height = 5, width = 6)

