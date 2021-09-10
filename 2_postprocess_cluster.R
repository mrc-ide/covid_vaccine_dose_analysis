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
df <- left_join(df, scenarios, by = "scenario")

# summarise over repetitions
df_summarise <- df %>%
  unnest(cols) %>%
  group_by(income_group, target_pop, R0, Rt1, Rt2, max_coverage, coverage, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase) %>%
mutate(deaths_med = median(deaths),
       deaths_lower = quantile(deaths, 0.025),
       deaths_upper = quantile(deaths, 0.975),
       prop_R_med = median(prop_R)) %>%
  ungroup() %>%
  select(-c(deaths, prop_R)) %>%
  group_by(timestep, income_group, target_pop, R0, Rt1, Rt2, max_coverage, coverage, vaccine_doses, vacc_start, variant_fold_reduction, dose_3_fold_increase, deaths_med, deaths_lower, deaths_upper, prop_R_med) %>%
  summarise(deaths_t = median(D_count),
            deaths_tmin = quantile(D_count, 0.025),
            deaths_tmax = quantile(D_count, 0.975),
            vaccines_t = median(X1_count + X2_count + X3_count),
            dose1_t = median(X1_count),
            dose2_t = median(X2_count),
            dose3_t = median(X3_count),
            prop_R = round(median(R_count)/target_pop * 100,2),
            .groups = 'drop') %>%
  unique()

counterfactual <- df_summarise %>%
  filter(max_coverage == 0) %>%
  select(timestep, income_group, R0, Rt1, Rt2, target_pop, deaths_t, deaths_tmin, deaths_tmax)

df1 <- filter(df_summarise, max_coverage == 0.8, income_group == "HIC")

# plot outputs: deaths, HIC, Wild-Type
ggplot(data = df1, aes(x = timestep, y = deaths_t, col = as.factor(vaccine_doses))) +
  geom_line() +
  geom_ribbon(data = filter(counterfactual, income_group == "HIC"), aes(ymin =deaths_tmin, ymax = deaths_tmax), alpha = 0.5, fill = "grey", col = NA) +
  geom_ribbon(aes(ymin = deaths_tmin, ymax = deaths_tmax, fill = as.factor(vaccine_doses)), alpha = 0.5, col = NA) +
  geom_line(data = filter(counterfactual, income_group == "HIC"), aes(x = timestep, y = deaths_t), col = "black", linetype = "dashed") +
  facet_wrap( ~ coverage, labeller = label_both) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c(col4, col2)) +
  scale_fill_manual(values = c(col4, col2)) +
  labs(x = "Timestep (days)", y = "Daily deaths", col = "Dose scenario", fill = "Dose scenario") +
  ggtitle("High income setting, WT")

#ggsave("plots/Fig1_HIC_WT_deaths.png", height = 4.5, width = 13)

# plot outputs, vaccine doses
df1_vacc <- df1 %>%
  pivot_longer(cols = c(dose1_t, dose2_t, dose3_t), names_to = "dose")

ggplot(data = df1_vacc, aes(x = timestep, y = value/target_pop*100, col = dose)) +
  geom_line() +
  facet_grid(as.factor(vaccine_doses) ~ coverage, labeller = label_both) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Timestep (days)", y = "Vaccinated (%)", col = "Dose number") +
  ggtitle("High income setting") +
  annotate(geom="text", x=500, y=50, xend=Inf, yend=Inf, label='DRAFT', color='grey', angle=45, fontface='bold', size=15, alpha=0.2, family='Arial')

#ggsave("plots/Fig3_HIC_vaccinated.png", height = 8, width = 13)

# summarise for bar plot
barplot_dat <- df %>%
  group_by(income_group, target_pop, hs_constraints, R0, Rt1, Rt2, vaccine_doses, max_coverage, coverage, vacc_start, variant_fold_reduction, dose_3_fold_increase) %>%
  summarise(deaths_med = median(deaths),
            deaths_upper = quantile(deaths, 0.975),
            deaths_lower = quantile(deaths, 0.025)) %>%
  filter(max_coverage != 0)

ggplot(data = barplot_dat, aes(x = coverage, y = deaths_med, fill = as.factor(vaccine_doses))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~income_group)
