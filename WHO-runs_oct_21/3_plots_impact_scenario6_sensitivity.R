icg <- "LMIC"

df_summarise <- readRDS("processed_outputs/df_summarise_scenario6_sensitivity.rds") %>%
  mutate(income_group = factor(income_group, levels = c("LMIC", "LIC"))) %>%
  filter(income_group == icg)
df_summarise_totals <- readRDS("processed_outputs/df_summarise_totals_scenario6_sensitivity.rds")%>%
  mutate(income_group = factor(income_group, levels = c("LMIC", "LIC"))) %>%
  filter(income_group == icg)

counterfactual <- df_summarise %>%
  filter(max_coverage == 0) %>%
  select(timestep, date, income_group, target_pop, deaths_t, deaths_tmin, deaths_tmax, vaccine_doses)

# plot outputs: total vaccinated over time
g0 <- ggplot(data = filter(df_summarise, max_coverage != 0, waning == "Default", vaccine_doses == "3 doses"), aes(x = as.Date(date), y  = vaccines_t/target_pop, linetype = rollout_rate, col = age_groups_covered)) +
  geom_line(size = 1) +
  lims(x = c(as.Date("2020-01-01"), as.Date("2023-07-01"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Cumulative doses delivered per million", col = "Dose scenario", linetype = "Rollout speed") +
  scale_color_manual(values = c("tomato3", col5))

g0

# plot outputs: deaths
g1 <- ggplot(data = filter(df_summarise, timestep >= 300, max_coverage != 0, rollout_rate == "Default", waning == "Default"), aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = vaccine_doses)) +
  geom_ribbon(data = counterfactual, aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = vaccine_doses), alpha = 0.3, col = NA) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = vaccine_doses), alpha = 0.3, col = NA) +
  geom_line(data = counterfactual, aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = vaccine_doses)) +
  geom_line() +
  facet_wrap( ~ age_groups_covered, nrow = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c(col6, col8, "black")) +
  scale_fill_manual(values = c(col6, col8, "grey")) +
  labs(x = "Year", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

g1

# barplot summary of deaths
df2 <- df_summarise_totals %>%
  mutate(sensitivity_scenario = if_else(waning == "Default" & rollout_rate == "Default", "Default", if_else(waning == "Default" & rollout_rate == "Slower rollout", "Slower rollout", if_else(waning == "Slower waning" & rollout_rate == "Default", "Slower waning", "None")))) %>%
  mutate(sensitivity_scenario = if_else(vaccine_doses == "Counterfactual", "Counterfactual", sensitivity_scenario)) %>%
  filter(sensitivity_scenario != "None")

df2_counter <- df2 %>%
  filter(max_coverage == 0)
df2_counter <- rbind(df2_counter, df2_counter)
df2_counter$age_groups_covered <- unique(df2$age_groups_covered)[1:2]

df2 <- df2 %>%
  filter(age_groups_covered != "Counterfactual") %>%
  rbind(df2_counter)

g2 <- ggplot(data = df2, aes(x = sensitivity_scenario, y = deaths_med/target_pop * 1e6, fill = vaccine_doses)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  facet_wrap( ~ age_groups_covered, nrow = 2) +
  scale_fill_manual(values = c("grey", col6, col8)) +
  labs(x = "Sensitivity scenario", y = "Total deaths per million since vaccination start", col = "", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0),
        legend.position = "none")
  
g2

# combine plots
library(patchwork)
combined <- g0 + guide_area() + g1  + g2 + plot_annotation(tag_levels = "A")+ plot_layout(guides = "collect") + plot_layout(widths = c(2,1), heights = c(1,2))
combined

ggsave("plots/scenario6_sensitivity.png", height = 10, width = 10)
