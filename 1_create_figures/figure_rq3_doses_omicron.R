name <- "rq3_hic_abmodel_zerocovid_omicron"

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

m <- unique(df_summarise$strategy_name)
m
df_summarise <- df_summarise %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))
df_summarise_totals <- df_summarise_totals %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))


df1 <- filter(df_summarise, vacc_per_week == 0.05, max_Rt == 5) %>%
  filter(strategy_name %in% c("10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+")) %>%
  mutate(Rt_lift_t = factor(Rt_lift_t, levels = c("Sept '21 lift", "Nov '21 lift", "Mar '22 lift"))) %>%
  filter(Rt_lift_t == "Sept '21 lift" | (Rt_lift_t == "Nov '21 lift" & (strategy_name %in% c("10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"))) | (Rt_lift_t == "Mar '22 lift" & strategy_name == "10y+ 2 doses, booster 10y+"))

df1 <- df1 %>%
  mutate(Rt_lift_m = if_else(Rt_lift_t == "Sept '21 lift", as.Date("2021-09-01"),
                             if_else(Rt_lift_t == "Nov '21 lift", as.Date("2021-11-01"), 
                                                                          if_else(Rt_lift_t == "Mar '22 lift", as.Date("2022-03-01"), as.Date("2022-12-01")))))
# plot total doses against Rt lifting to check timing
ggplot(data = df1, aes(x = as.Date(date), y = vaccines_t / target_pop)) +
  geom_line() +
  geom_line(aes(x = as.Date(date), y = Rt), col = "red") +
  facet_grid(Rt_lift_t ~ strategy_name) +
  labs(x = "Date. Note red line shows lifting Rt", y = "Doses delivered per person")
ggsave("plots/doses_vs_lifting_Rt_rq3.png", height = 6, width = 10)

#################################################
# blue-green doses barplot
df_doses <- df1 %>%
  filter(vaccine_doses != "Pre-vaccine introduction") %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose")

df1_doses_month <- df_doses %>%
  # filter to last date of each month
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         date = lubridate::floor_date(date, "month")) %>%
  group_by(income_group, target_pop, age_groups_covered, vaccine_doses, dose, year, month, Rt_lift_m) %>% 
  mutate(max_day = max(day)) %>%
  ungroup() %>%
  filter(day == max_day)

p3 <- ggplot(data = df1_doses_month, aes(x = as.Date(date), y = value/target_pop*100, fill = dose)) +
  geom_bar(stat = "identity") +
  geom_vline(aes(xintercept = Rt_lift_m, linetype = Rt_lift_t, col = Rt_lift_t), size = 0.8) +
  facet_grid( ~ strategy_name) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.3, hjust=0.2)) +
  scale_fill_brewer(palette = "Paired") +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  scale_color_manual(values = c("seagreen", "black", "darkred")) +
  labs(x = "Time", y = "Vaccinated (%)", fill = "Dose number", col = "Rt lift", linetype = "Rt lift")
p3
ggsave("plots/rq3_hic_doses_lift.png", p3, width = 8, height = 4)
