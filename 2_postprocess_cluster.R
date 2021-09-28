source("R/plotting_utils.R")

# join the runs and link to parameters
scenarios <- read_csv("scenarios.csv", show_col_types = FALSE)
df <- list.files(path = "output_cluster/", pattern = ".rds")
df <- map(paste0("output_cluster/", df), readRDS)
df <- do.call(rbind,df)
df <- left_join(df, scenarios, by = "scenario") 

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
            dose1_t = median(X1_count),
            dose2_t = median(X2_count),
            dose3_t = median(X3_count),
            prop_R = round(median(R_count)/target_pop * 100,2),
            .groups = 'drop') %>%
  unique()

counterfactual <- df_summarise %>%
  filter(max_coverage == 0) %>%
  select(timestep, income_group, R0, Rt1, Rt2, target_pop, deaths_t, deaths_tmin, deaths_tmax)

df1 <- filter(df_summarise, max_coverage == 0.8, income_group == "HIC")%>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(5,8), labels = c("60+ years", "45+ years")))

# plot outputs: deaths, HIC
ggplot(data = df1, aes(x = timestep, y = deaths_t/target_pop * 1e6, col = as.factor(vaccine_doses))) +
   geom_ribbon(data = filter(counterfactual, income_group == "HIC"), aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6), alpha = 0.5, fill = "grey", col = NA) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = as.factor(vaccine_doses)), alpha = 0.3, col = NA) +
  geom_line(data = filter(counterfactual, income_group == "HIC"), aes(x = timestep, y = deaths_t/target_pop * 1e6), col = "black") +
  geom_line() +
  facet_wrap( ~ age_groups_covered) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c(col6, col8)) +
  scale_fill_manual(values = c(col6, col8)) +
  labs(x = "Timestep (days)", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario") +
  ggtitle("High income setting") +
  annotate(geom="text", x=500, y=60, xend=Inf, yend=Inf, label='DRAFT', color='grey', angle=45, fontface='bold', size=15, alpha=0.2, family='Arial')

ggsave("plots/Fig1_HIC_WT_deaths.png", height = 4.5, width = 13)


######################################################################
counterfactual <- df_summarise %>%
  filter(max_coverage == 0) %>%
  select(timestep, income_group, R0, Rt1, Rt2, target_pop, deaths_t, deaths_tmin, deaths_tmax) %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses")))

df2 <- filter(df_summarise, max_coverage == 0.8, income_group == "LMIC")%>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(5,8), labels = c("60+ years", "45+ years")))# %>%
  #filter(age_groups_covered == "45+ years")

# plot outputs: deaths, LMIC
ggplot(data = df2, aes(x = timestep, y = deaths_t/target_pop * 1e6, col = as.factor(vaccine_doses))) +
  geom_ribbon(data = filter(counterfactual, income_group == "LMIC"), aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6), alpha = 0.5, fill = "grey", col = NA) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = as.factor(vaccine_doses)), alpha = 0.3, col = NA) +
  geom_line(data = filter(counterfactual, income_group == "LMIC"), aes(x = timestep, y = deaths_t/target_pop * 1e6), col = "black") +
  geom_line() +
  facet_wrap( ~ age_groups_covered) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c(col6, col8)) +
  scale_fill_manual(values = c(col6, col8)) +
  labs(x = "Timestep (days)", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario") +
  ggtitle("Lower-middle income setting") +
  annotate(geom="text", x=500, y=100, xend=Inf, yend=Inf, label='DRAFT', color='grey', angle=45, fontface='bold', size=15, alpha=0.2, family='Arial')

ggsave("plots/Fig1_LMIC_WT_deaths.png", height = 4.5, width = 8)

##############################################################################
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
