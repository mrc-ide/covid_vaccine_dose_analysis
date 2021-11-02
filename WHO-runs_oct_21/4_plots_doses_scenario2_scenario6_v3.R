df_summarise <- readRDS("processed_outputs/df_summarise_scenario2.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_scenario6.rds")) %>%
  mutate(income_group = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC")))

df1_vacc <- filter(df_summarise, max_coverage == 0.8) %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose")


df1_vacc_month <- df1_vacc %>%
  # filter to last date of each month
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         date = lubridate::floor_date(date, "month")) %>%
  group_by(income_group, target_pop, age_groups_covered, vaccine_doses, dose, year, month) %>% 
  mutate(max_day = max(day)) %>%
  ungroup() %>%
  filter(day == max_day)

# plot outputs, vaccine doses

g1 <- ggplot(data = df1_vacc_month, aes(x = as.Date(date), y = value/target_pop*100, fill = dose)) +
  geom_bar(stat = "identity") +
  facet_grid(income_group ~ age_groups_covered + vaccine_doses, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Time (year)", y = "Vaccinated .(%)", col = "Dose number")
g1

ggsave("plots/research_question_2_doses.png", height = 8, width = 10)


