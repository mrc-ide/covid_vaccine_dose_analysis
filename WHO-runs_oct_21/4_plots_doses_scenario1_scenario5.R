df_summarise <- readRDS("processed_outputs/df_summarise_scenario1.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_scenario5.rds")) %>%
  mutate(income_group = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC")))

df1 <- filter(df_summarise, max_coverage == 0.8) %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) 
# plot outputs, vaccine doses
df1_vacc <- df1 %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose") %>%
  filter(strategy_name != "Counterfactual")

p1 <- ggplot(data = filter(df1_vacc, income_group %in% c("HIC", "UMIC")), aes(x = as.Date(date), y = value/target_pop*100, col = dose)) +
  geom_line(size = 0.8) +
  facet_grid( income_group ~ strategy_name) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Timestep (days)", y = "Vaccinated .(%)", col = "Dose number")
p1
p2 <- ggplot(data = filter(df1_vacc, income_group %in% c("LMIC", "LIC")), aes(x = as.Date(date), y = value/target_pop*100, col = dose)) +
  geom_line(size = 0.8) +
  facet_grid( income_group ~ strategy_name) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Timestep (days)", y = "Vaccinated .(%)", col = "Dose number")
p2

combined <- p1 / p2+ plot_annotation(tag_levels = 'A')
combined

ggsave("plots/scenario1_scenario5_doses.png", height = 10, width = 10)


