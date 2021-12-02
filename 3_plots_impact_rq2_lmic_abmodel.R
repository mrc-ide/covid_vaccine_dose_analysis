name <- "rq2_lmic_abmodel"
ages_covered <- 9

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered)

df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered)

df_summarise_totals <- df_summarise_totals %>%
  mutate(max_Rt = factor(max_Rt, levels = c(5,3), labels = c("Rt_max = 5", "Rt_max = 3")))

# plot outputs: total vaccinated over time
g0 <- ggplot(data = filter(df_summarise, vaccine_doses == "3 doses"), aes(x = as.Date(date), y  = vaccines_t/target_pop, linetype = rollout_rate)) +
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
g1 <- ggplot(data = filter(df_summarise, vacc_per_week == 0.02, max_Rt == 5), aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = vaccine_doses)) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = vaccine_doses), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c(col6, col8, "black")) +
  scale_fill_manual(values = c(col6, col8, "grey")) +
  labs(x = "Time", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

g1

# plot outputs: incidence
g1b <- ggplot(data = filter(df_summarise, vacc_per_week == 0.02, timestep < max(df_summarise$timestep), max_Rt == 3), aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = vaccine_doses)) +
  geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = vaccine_doses), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c(col6, col8, "black")) +
  scale_fill_manual(values = c(col6, col8, "grey")) +
  labs(x = "Time", y = "Daily incidence per million", col = "Dose scenario", fill = "Dose scenario")

g1b

# barplot summary of deaths
g2 <- ggplot(data = df_summarise_totals, aes(x = rollout_rate, y = deaths_med/target_pop * 1e6, fill = vaccine_doses)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  #geom_errorbar(aes(ymin = deaths_lower/target_pop * 1e6, ymax = deaths_upper / target_pop * 1e6), position = position_dodge(1), width = 0.5) +
  scale_fill_manual(values = c(col6, col8)) +
  labs(x = "Sensitivity scenario", y = "Deaths per mill since vaccination", col = "", fill = "Dose scenario") +
  facet_wrap(~ max_Rt) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.7, hjust=0.3)
        )
  
g2

g2b <- ggplot(data = df_summarise_totals, aes(x = rollout_rate, y = inc_med/target_pop * 1e6, fill = vaccine_doses)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  #geom_errorbar(aes(ymin = inc_lower/target_pop * 1e6, ymax = inc_upper / target_pop * 1e6), position = position_dodge(1), width = 0.5) +
  scale_fill_manual(values = c(col6, col8)) +
  labs(x = "Sensitivity scenario", y = "Incidence per mill since vaccination", col = "", fill = "Dose scenario") +
  facet_wrap(~ max_Rt) +
  
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.7, hjust=0.3)
  )

g2b

#################################################
# blue-green doses barplot
df_doses <- df_summarise %>%
  filter(vaccine_doses != "Pre-vaccine introduction") %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  filter(rollout_rate == "Default") %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose")

df1_doses_month <- df_doses %>%
  # filter to last date of each month
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         date = lubridate::floor_date(date, "month")) %>%
  group_by(income_group, target_pop, age_groups_covered, vaccine_doses, dose, year, month) %>% 
  mutate(max_day = max(day)) %>%
  ungroup() %>%
  filter(day == max_day)

p3 <- ggplot(data = df1_doses_month, aes(x = as.Date(date), y = value/target_pop*100, fill = dose)) +
  geom_bar(stat = "identity") +
  facet_grid( ~ vaccine_doses) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.3, hjust=0.2)) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Time", y = "Vaccinated (%)", fill = "Dose number")
p3

# combine plots
library(patchwork)

layout <- "
AABB
CCDE
"
combined <- g1 + p3  + g1b  + g2 + g2b +
  plot_annotation(tag_levels = "A")+ plot_layout(guides = "collect") + plot_layout(design = layout)
combined

ggsave(paste0("plots/impact_", name, "_ages_cov_", ages_covered, ".png"), height = 8, width = 16)

