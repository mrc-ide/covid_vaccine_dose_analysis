df_summarise <- readRDS("processed_outputs/df_summarise_scenario2.rds") %>%
  filter(income_group == "HIC")
df_summarise_totals <- readRDS("processed_outputs/df_summarise_totals_scenario2.rds")

counterfactual <- df_summarise %>%
  filter(income_group == "HIC")%>%
  filter(max_coverage == 0) %>%
  select(timestep, date, income_group, target_pop, deaths_t, deaths_tmin, deaths_tmax)

# plot outputs: deaths
ggplot(data = filter(df_summarise, timestep >= 300, max_coverage != 0), aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = vaccine_doses)) +
  geom_ribbon(data = counterfactual, aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6), alpha = 0.5, fill = "grey", col = NA) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = vaccine_doses), alpha = 0.3, col = NA) +
  #geom_line(data = counterfactual, aes(x = as.Date(date), y = deaths_t/target_pop * 1e6), col = "black") +
  geom_line() +
  facet_wrap(ab_model_infection ~ age_groups_covered) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c(col6, col8, col1)) +
  scale_fill_manual(values = c(col6, col8, col1)) +
  labs(x = "Timestep (days)", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

ggsave("plots/scenario2_impact_time.png", height = 8, width = 10)

# barplot summary of deaths
ggplot(data = df_summarise_totals, aes(x = age_groups_covered, y = deaths_med/target_pop * 1e6, fill = vaccine_doses)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~income_group) +
  scale_fill_manual(values = c("grey", col6, col8)) +
  labs(x = "Age groups targeted for initial first 2 doses", y = "Total deaths per million since vaccination start", col = "", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0))
  
ggsave("plots/scenario2_summary.png", height = 4.5, width = 8)



