library(patchwork)
library(lubridate)

df_summarise <- readRDS("processed_outputs/df_summarise_rq1_lmic.rds")

df1 <- filter(df_summarise, max_coverage == 0.8) %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) %>%
  filter(vacc_per_week == 0.015,
         waning == "Default")

# plot outputs, vaccine doses
df1_vacc <- df1 %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose") %>%
  filter(strategy_name != "Counterfactual")

df1_vacc_month <- df1_vacc %>%
  # filter to last date of each month
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         date = lubridate::floor_date(date, "month")) %>%
  group_by(income_group, target_pop, strategy_name, dose, year, month) %>% 
  mutate(max_day = max(day)) %>%
  ungroup() %>%
  filter(day == max_day)

p2 <- ggplot(data = filter(df1_vacc_month, income_group %in% c("LMIC", "LIC")), aes(x = as.Date(date), y = value/target_pop*100, fill = dose)) +
  geom_bar(stat = "identity") +
  facet_grid(  ~ strategy_name) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Time (year)", y = "Vaccinated .(%)", fill = "Dose number")
p2

ggsave("plots/rq1_doses_lmic.png", height = 5, width = 10)


