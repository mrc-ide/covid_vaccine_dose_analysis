icg = "HIC"

df_summarise <- readRDS("processed_outputs_old/df_summarise_scenario1.rds") %>%
  filter(!(strategy_name != "Counterfactual" & timestep < 300)) %>%
  filter(income_group == icg)

df_summarise_totals <- readRDS("processed_outputs_old/df_summarise_totals_scenario1.rds") %>%
  filter(income_group == icg)

counterfactual <- df_summarise %>%
  filter(max_coverage == 0) %>%
  select(timestep, date, income_group, target_pop, deaths_t, deaths_tmin, deaths_tmax)

# plot outputs: deaths
p1 <- ggplot(data = df_summarise, aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("black", col6, col8, col1)) +
  scale_fill_manual(values = c("darkgrey", col6, col8, col1)) +
  labs(x = "Year", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")
p1
ggsave(paste0("plots/scenario1_impact_time_", icg, ".png"), height = 4.5, width = 10)

# barplot summary of deaths
p2 <- ggplot(data = df_summarise_totals, aes(x = strategy_name, y = deaths_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("grey", col6, col8, col1)) +
  labs(x = "Dose scenario", y = "Total deaths per million since vaccination start", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0))
p2 
ggsave(paste0("plots/scenario1_summary_", icg, ".png"), height = 4.5, width = 5)

# summary of deaths averted per dose
df_summarise_averted <- df_summarise_totals
df_summarise_averted_0 <- df_summarise_totals %>%
  filter(max_coverage == 0) %>%
  select(income_group, hs_constraints, deaths_med) %>%
  rename(deaths_med_counter = deaths_med)
df_summarise_averted <- df_summarise_averted %>%
  left_join(df_summarise_averted_0) %>%
  mutate(deaths_averted = deaths_med_counter - deaths_med) %>%
  filter(max_coverage != 0)

p3 <- ggplot(data = df_summarise_averted, aes(x = strategy_name, y = deaths_averted/total_doses_med * 100, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c(col6, col8, col1)) +
  labs(x = "Dose scenario", y = "Total deaths averted per 100 doses", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0))

p3
ggsave(paste0("plots/scenario1_summary_perdose", icg, ".png"), height = 4.5, width = 5)

p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")
combined <- p1 / (p2 | p3) + plot_annotation(tag_levels = "A")+ plot_layout(guides = "collect")
combined
ggsave("plots/scenario1_summary.png", height = 8, width = 10)

# plot vaccines delivered over time
# show total doses over time as a check
df2 <- filter(df_summarise, max_coverage == 0.8) 
p0 <- ggplot(data = df2, aes(x = as.Date(date), y  = vaccines_t/target_pop, col = strategy_name)) +
  geom_line(size = 1) +
  lims(x = c(as.Date("2020-01-01"), as.Date("2023-01-01"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Cumulative doses delivered per million", col = "Dose scenario") +
scale_color_manual(values = c(col6, col8, col1))
  
p0

combined <- p0 / p1 / (p2 | p3) + plot_annotation(tag_levels = "A")+ plot_layout(guides = "collect")
combined
ggsave(paste0("plots/scenario1_summary_", icg, ".png"), height = 8, width = 10)
