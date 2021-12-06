name <- "rq3_hic_abmodel_zerocovid"

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

m <- unique(df_summarise$strategy_name)
m
df_summarise <- df_summarise %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))
df_summarise_totals <- df_summarise_totals %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))


df1 <- filter(df_summarise, vacc_per_week == 0.05) %>%
  filter(strategy_name %in% c("10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+")) %>%
  mutate(Rt_lift_t = factor(Rt_lift_t, levels = c("Sept '21 lift", "Nov '21 lift", "Mar '22 lift"))) %>%
  filter(Rt_lift_t == "Sept '21 lift" | (Rt_lift_t == "Nov '21 lift" & (strategy_name %in% c("10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"))) | (Rt_lift_t == "Mar '22 lift" & strategy_name == "10y+ 2 doses, booster 10y+"))

df_summarise_totals <- df_summarise_totals %>%
  mutate(max_Rt = factor(max_Rt, levels = c(5,3), labels = c("Rt_max = 5", "Rt_max = 3")))

df2 <- filter(df_summarise_totals, vacc_per_week == 0.05) %>%
  filter(strategy_name %in% c("10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+")) %>%
  mutate(Rt_lift_t = factor(Rt_lift_t, levels = c("Sept '21 lift", "Nov '21 lift", "Mar '22 lift"))) %>%
  filter(Rt_lift_t == "Sept '21 lift" | (Rt_lift_t == "Nov '21 lift" & (strategy_name %in% c("10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"))) | (Rt_lift_t == "Mar '22 lift" & strategy_name == "10y+ 2 doses, booster 10y+"))

# plot outputs: deaths
g1 <- ggplot(data = df1, aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~ Rt_lift_t, nrow = 3) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none") +
  scale_color_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  labs(x = "Year", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

g1

######################################################
# barchart
g2 <- ggplot(data = df2, aes(x = Rt_lift_t, y = deaths_med / target_pop * 1e6, fill = strategy_name)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  #geom_errorbar(aes(ymin = deaths_lower/target_pop * 1e6, ymax = deaths_upper / target_pop * 1e6), position = position_dodge2(0.5, preserve = "single", width = 0.5)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  labs(x = "Rt lift scenario", y = "Total deaths per million", fill = "Dose scenario")

g2

g2b <- ggplot(data = df2, aes(x = Rt_lift_t, y = inc_med / target_pop * 1e6, fill = strategy_name)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  #geom_errorbar(aes(ymin = inc_lower/target_pop * 1e6, ymax = inc_upper / target_pop * 1e6), position = position_dodge2(0.5, preserve = "single", width = 0.5)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  labs(x = "Rt lift scenario", y = "Total incidence per million", fill = "Dose scenario")

g2b



library(patchwork)
combined <- g1 + (g2 / g2b) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") +
  plot_layout(widths = c(2, 1))

combined

ggsave(paste0("plots/plots_impact_", name, "_Rt_5.png"), combined, height = 10, width = 15)
