df_summarise <- readRDS("processed_outputs/df_summarise_scenario2.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_scenario6.rds")) %>%
  mutate(income_group = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC")))

df1_vacc <- filter(df_summarise, max_coverage == 0.8) %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose")

# plot outputs, vaccine doses

ggplot(data = df1_vacc, aes(x = as.Date(date), y = value/target_pop*100, col = dose)) +
  geom_line(size = 0.8) +
  facet_grid(income_group ~ age_groups_covered + vaccine_doses, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Timestep (days)", y = "Vaccinated .(%)", col = "Dose number")

ggsave("plots/scenario2_scenario6_doses.png", height = 8, width = 10)


