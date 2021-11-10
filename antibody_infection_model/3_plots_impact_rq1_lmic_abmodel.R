df_summarise <- readRDS("processed_outputs/ab_infection_model/df_summarise_rq1_lmic_abmodel.rds")
df_summarise_totals <- readRDS("processed_outputs/ab_infection_model/df_summarise_totals_rq1_lmic_abmodel.rds")

df0 <- df_summarise %>%
  filter(period_s == 250,
         t_period_l == 365,
         t_d3 == 240,
         vacc_per_week == 0.015)

df0_pre_vacc <- df0 %>%
  filter(date <= as.Date("2021-01-01")) %>%
  filter(strategy_name == "40y+ 2 doses") %>%
  mutate(strategy_name = "Pre-vaccine introduction")

df0_post_vacc <- df0 %>%
  filter(date >= as.Date("2021-01-01"))

df0_both <- rbind(df0_pre_vacc, df0_post_vacc)
m <- unique(df0_both$strategy_name)
df0_both <- df0_both %>%
  mutate(strategy_name = factor(strategy_name, levels = m[c(1,3,2,4)]))

# plot total doses over time
p0 <- ggplot(data = df0, aes(x = as.Date(date), y  = vaccines_t/target_pop, col = strategy_name)) +
  geom_line(size = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Cumulative doses delivered per person", col = "Dose scenario") +
  scale_color_manual(values = c(col6, col8, col1))
p0

# plot outputs: deaths
p1 <- ggplot(data = df0_both, aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("black", col6, col8, col1, "black")) +
  scale_fill_manual(values = c("darkgrey", col6, col8, col1, "darkgrey")) +
  labs(x = "Timestep (days)", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")
p1

# barplot summary of deaths
df2 <- df_summarise_totals %>%
  mutate(sensitivity_scenario = if_else(waning == "Default" & rollout_rate == "Default" & t_d3 == 240, "Default", if_else(waning == "Default" & rollout_rate == "Slower rollout" & t_d3 == 240, "Slower rollout", if_else(waning == "Slower waning" & rollout_rate == "Default" & t_d3 == 240, "Slower waning", "None")))) %>%
  mutate(sensitivity_scenario = if_else(waning == "Default" & rollout_rate == "Default" & t_d3 == 180, "Booster 6 months", if_else(waning == "Default" & rollout_rate == "Default" & t_d3 == 360, "Booster 12 months", sensitivity_scenario))) %>%
  filter(sensitivity_scenario != "None") %>%
  mutate(sensitivity_scenario = factor(sensitivity_scenario, levels = c("Default", "Slower rollout", "Slower waning", "Booster 6 months", "Booster 12 months")))

p2 <- ggplot(data = df2, aes(x = sensitivity_scenario, y = deaths_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  #geom_errorbar(aes(ymin = deaths_lower/target_pop * 1e6, ymax = deaths_upper / target_pop * 1e6), position = position_dodge()) +
  scale_fill_manual(values = c(col6, col8, col1, "grey")) +
  labs(x = "Dose scenario", y = "Total deaths per million since vaccination start", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0))
p2

p0 <- p0 + theme(legend.position = "none")
p2 <- p2 + theme(legend.position = "none")
combined <- p0 / p2 + p1 + guide_area() + plot_annotation(tag_levels = "A")+ plot_layout(guides = "collect") + plot_layout(widths = c(2,1)) + plot_layout(ncol = 2, nrow = 2)
combined
ggsave("plots/plots_impact_rq1_lmic_abmodel.png", height = 8, width = 10)
