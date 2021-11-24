name <- "rq1_hic_abmodel"

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

m <- unique(df_summarise$strategy_name)
m
df_summarise <- df_summarise %>%
  mutate(strategy_name = factor(strategy_name, levels = c(m[1], m[2], m[3], m[4], m[5], m[6]), ordered = TRUE))

# plot total doses over time
p0 <- ggplot(data = filter(df_summarise,
                           rollout_rate == "Default",
                           waning == "Default",
                           dose_3_timing == "6 months (default)",
                           strategy_name != "Pre-vaccine introduction"), aes(x = as.Date(date), y  = vaccines_t/target_pop, col = strategy_name)) +
  geom_line(size = 1) +
  #facet_wrap(~t_d3, nrow = 3)+
  lims(x = c(as.Date("2020-01-01"), as.Date("2023-01-01"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Cumulative doses delivered per person", col = "Dose scenario") +
  scale_color_viridis_d(direction = -1)

p0

# plot outputs: deaths
p1 <- ggplot(data = filter(df_summarise,
                           rollout_rate == "Default",
                           waning == "Default",
                           dose_3_timing == "6 months (default)",
                           strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"))
             , aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
 # facet_wrap(~t_d3, nrow = 3)+
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Time", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")
p1

# barplot summary of deaths
df2 <- df_summarise_totals %>%
  mutate(sensitivity_scenario = if_else(waning == "Default" & rollout_rate == "Default" & dose_3_timing == "6 months (default)", "Default", if_else(waning == "Default" & rollout_rate == "Slower rollout" & dose_3_timing == "6 months (default)", "Slower rollout", if_else(waning == "Slower vaccine waning" & rollout_rate == "Default" & dose_3_timing == "6 months (default)", "Slower vaccine waning", "None")))) %>%
  mutate(sensitivity_scenario = if_else(waning == "Default" & rollout_rate == "Default" & dose_3_timing == "12 months", "Booster 12 months", sensitivity_scenario)) %>%
  filter(sensitivity_scenario != "None") %>%
  mutate(sensitivity_scenario = factor(sensitivity_scenario, levels = c("Default", "Slower rollout", "Slower vaccine waning", "Booster 12 months")))

p2 <- ggplot(data = filter(df2, sensitivity_scenario %in% c("Default", "Slower vaccine waning")), aes(x = sensitivity_scenario, y = deaths_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = deaths_lower/target_pop * 1e6, ymax = deaths_upper / target_pop * 1e6), position = position_dodge()) +
  #scale_fill_manual(values = c(col6, col8, col1, "grey")) +
  labs(x = "Dose scenario", y = "Total deaths per million since vaccination start", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0))
p2

# plot outputs: infections
p3 <- ggplot(data = filter(df_summarise,
                           rollout_rate == "Default",
                           waning == "Default",
                           dose_3_timing == "6 months (default)",
                           strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"))
             , aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Time", y = "Daily incidence per million", col = "Dose scenario", fill = "Dose scenario")
p3

# plot outputs: deaths with wider dose spacing
p4 <- ggplot(data = filter(df_summarise,
                           rollout_rate == "Default",
                           waning == "Default",
                           strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, booster 60y+"))
             , aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = dose_3_timing)) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = dose_3_timing), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_viridis_d(option = "C", direction = -1) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  labs(x = "Time", y = "Daily deaths per million", col = "Booster dose timing", fill = "Booster dose timing")
p4

################################
name <- "rq1_hic_child_abmodel"

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

p5 <- ggplot(data = filter(df_summarise_totals,
                           waning == "Default"), aes(x = strategy_name, y = deaths_since_22_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = deaths_since_22_lower/target_pop * 1e6, ymax = deaths_since_22_upper / target_pop * 1e6), position = position_dodge()) +
  facet_wrap(~transmission) +
  #scale_fill_manual(values = c(col6, col8, col1, "grey")) +
  labs(x = "Dose scenario", y = "Total deaths per million since vaccination start", col = "Dose scenariðo", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0))
p5

p6 <- ggplot(data = filter(df_summarise_totals,
                           waning == "Default"), aes(x = strategy_name, y = inc_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = inc_lower/target_pop * 1e6, ymax = inc_upper / target_pop * 1e6), position = position_dodge()) +
  #scale_fill_manual(values = c(col6, col8, col1, "grey")) +
  labs(x = "Dose scenario", y = "Total cases per million since vaccination start", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  facet_wrap(~transmission) +
  
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0))
p6

p7 <- ggplot(data = filter(df_summarise,
                           rollout_rate == "Default",
                           waning == "Default",
                           dose_3_timing == "6 months (default)")
             , aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +  facet_wrap(~transmission) +
  facet_wrap(~transmission) +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Time", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")
p7

# plot total doses over time
p8 <- ggplot(data = filter(df_summarise,
                           rollout_rate == "Default",
                           waning == "Default",
                           dose_3_timing == "6 months (default)",
                           strategy_name != "Pre-vaccine introduction"), aes(x = as.Date(date), y  = vaccines_t/target_pop, col = strategy_name)) +
  geom_line(size = 1) +
  #facet_wrap(~t_d3, nrow = 3)+
  lims(x = c(as.Date("2020-01-01"), as.Date("2023-01-01"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Cumulative doses delivered per person", col = "Dose scenario") +
  scale_color_viridis_d(direction = -1)

p8
#p0 <- p0 + theme(legend.position = "none")
#p2 <- p2 + theme(legend.position = "none")

library(patchwork)
combined <- p0 + p2 + p1 + age_plot + p3 + p4 +
  plot_annotation(tag_levels = "A") + 
  #plot_layout(guides = "collect") + 
 # plot_layout(widths = c(2,1)) + 
  plot_layout(ncol = 2, nrow = 3)

combined

ggsave(paste0("plots/plots_impact_", name, ".png"), height = 15, width = 20)
