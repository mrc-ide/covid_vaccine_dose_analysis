name <- "rq2_lmic_abmodel"
ages_covered <- 5
dose_3_t <- 180

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered,
         t_d3 == dose_3_t)

df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered)

#################################################
# blue-green doses barplot
df_doses <- df_summarise %>%
  filter(vaccine_doses != "Pre-vaccine introduction",
         t_d3 == 180) %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Booster" = "dose3_t") %>%
  filter(rollout_rate == "Default") %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Booster"), names_to = "dose") %>%
  mutate(dose = factor(dose, levels = c("Dose 1", "Dose 2", "Booster"), ordered = TRUE))

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

plot_doses <- ggplot(data = df1_doses_month, aes(x = as.Date(date), y = value/target_pop*100, fill = dose)) +
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
plot_doses

########################################
#deaths delta
deaths_delta <- ggplot(data = filter(df_summarise, vacc_per_week == 0.02, max_Rt == 5), aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = vaccine_doses)) +
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

deaths_delta

# plot outputs: incidence
inc_delta <- ggplot(data = filter(df_summarise, vacc_per_week == 0.02, timestep < max(df_summarise$timestep)), aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = vaccine_doses)) +
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

inc_delta

#################################################
# omicron plots
name <- "rq2_lmic_abmodel_omicron"

df_summarise_om <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered,
         t_d3 == dose_3_t) %>%
  mutate(vfr_lab = factor(vfr, levels = c(2,4,8,16), labels = c("VFR = 2", "VFR = 4", "VFR = 8", "VFR = 16"), ordered = TRUE))

df_summarise_totals_om <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered) 

deaths_omicron <- ggplot(data = filter(df_summarise_om, vacc_per_week == 0.02), aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = vaccine_doses)) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = vaccine_doses), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~vfr_lab, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c(col6, col8, "black")) +
  scale_fill_manual(values = c(col6, col8, "grey")) +
  labs(x = "Time", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

deaths_omicron

#########################
# summary barchart
df_om <- df_summarise_totals_om %>%
  select(deaths_med, inc_med, vfr, vaccine_doses, rollout_rate, dose_3_timing) %>%
  mutate(scenario = paste0("Omicron: \nVFR = ", vfr))

df_delta <- df_summarise_totals %>%
  select(deaths_med, inc_med, vaccine_doses, rollout_rate, dose_3_timing) %>%
  mutate(vfr = 1,
         scenario = "Delta")

vfr <- c(2,4,8,16)
df_barchart <- rbind(df_delta, df_om)
df_barchart$scenario <- factor(df_barchart$scenario, levels = c("Delta", paste0("Omicron: \nVFR = ", vfr)), ordered = TRUE)

df_barchart <-df_barchart %>%
  filter(!(dose_3_timing == "12 months" & rollout_rate == "Slower rollout")) %>%
  mutate(sensitivity_scenario = if_else(dose_3_timing == "6 months (default)" & rollout_rate == "Default", "Default", if_else(dose_3_timing == "6 months (default)" & rollout_rate == "Slower rollout", "Slower rollout", if_else(dose_3_timing == "12 months" & rollout_rate == "Default", "12 months to booster", "NA")))) %>%
  filter(sensitivity_scenario != "NA") %>%
  mutate(sensitivity_scenario = factor(sensitivity_scenario, levels = c("Default", "Slower rollout", "12 months to booster")))

# barplot summary of deaths
p_deaths_summary <- ggplot(data = df_barchart, aes(x = scenario, y = deaths_med/target_pop * 1e6, fill = vaccine_doses)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  labs(x = "Variant scenario", y = "Total deaths per million", col = "Dose scenario", fill = "Dose scenario") +
  facet_wrap(~sensitivity_scenario) +
  scale_fill_manual(values = c(col6, col8)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_deaths_summary

# barplot summary of incidence
p_inc_summary <- ggplot(data = df_barchart, aes(x = scenario, y = inc_med/target_pop * 1e6, fill = vaccine_doses)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  labs(x = "Variant scenario", y = "Total incidence per million", col = "Dose scenario", fill = "Dose scenario") +
  facet_wrap(~sensitivity_scenario) +
  scale_fill_manual(values = c(col6, col8)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_inc_summary
ggsave(paste0("plots/fig3_inc_age_groups_covered_", ages_covered, ".png"),p_inc_summary, height = 4, width = 11)

# combine plots
library(patchwork)
layout <- "
AB
CC
DD
"
combined <- plot_doses + deaths_delta + p_deaths_summary + deaths_omicron +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 3, design = layout)

combined
ggsave(paste0("plots/fig3_age_groups_covered_", ages_covered, ".png"),combined, height = 10, width = 11)

