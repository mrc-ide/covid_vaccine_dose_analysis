df_summarise <- readRDS("processed_outputs/df_summarise_eu.rds")
df_summarise_totals <- readRDS("processed_outputs/df_summarise_totals_eu.rds")

counterfactual <- df_summarise %>%
  filter(max_coverage == 0) %>%
  select(timestep, date, income_group, target_pop, deaths_t, deaths_tmin, deaths_tmax)

# plot the counterfactual
g1 <- ggplot(data = counterfactual, aes(x = as.Date(date), y = deaths_t)) +
  geom_line() +
  facet_wrap(~income_group)
g1

df1 <- filter(df_summarise, max_coverage == 0.8, income_group == "HIC")%>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(5,9), labels = c("60+ years", "40+ years")))

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

ggsave("plots/Fig1_HIC_deaths_update.png", height = 4.5, width = 13)


######################################################################
counterfactual <- df_summarise %>%
  filter(max_coverage == 0) %>%
  select(timestep, income_group, R0, Rt1, Rt2, target_pop, deaths_t, deaths_tmin, deaths_tmax) %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses")))

df2 <- filter(df_summarise, max_coverage == 0.8, income_group == "LMIC")%>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(5,9), labels = c("60+ years", "40+ years")))

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
  annotate(geom="text", x=500, y=30, xend=Inf, yend=Inf, label='DRAFT', color='grey', angle=45, fontface='bold', size=15, alpha=0.2, family='Arial')

ggsave("plots/Fig1_LMIC_deaths_update.png", height = 4.5, width = 8)

