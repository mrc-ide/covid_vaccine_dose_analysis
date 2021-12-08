##############################################
# AGE PLOT
name <- "rq1_hic_abmodel_age"
df_age <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  unique()

x <- seq(0,80,5)
y <- seq(4,84,5)
z <- paste0(x,"-",y)
z[17] <- "80+"

age_plot <- ggplot(data = df_age, aes(x = age, y= value_med, fill =strategy_name)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0),
        legend.position = "none") +
  labs(x = "Age", y = "Total deaths", fill = "Dose scenario") +
  scale_x_continuous(breaks = 1:17, labels = z) +
  scale_fill_manual(values = col_set)

age_plot

##############################################
# HEATMAP
name <- "rq1_hic_omicron_heatmap"
df_heatmap <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))

heatmap_plot <- ggplot(data = df_heatmap, aes(x = factor(max_Rt_omicron), y = factor(vfr), fill = deaths_med)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Rt_max", y = "Variant fold reduction", fill = "Total deaths") 
heatmap_plot

##############################################

# Impact plots
name <- "rq1_hic_abmodel"

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

m <- unique(df_summarise$strategy_name)
m

df_summarise <- df_summarise %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))

df_summarise_totals <- df_summarise_totals %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))

df_summarise <- df_summarise %>%
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("Pre-vaccine introduction", "6 months (default)", "12 months"), ordered = TRUE))

df_summarise_totals <- df_summarise_totals %>%
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("6 months (default)", "12 months"), ordered = TRUE))

# numbers for text
x <- df_summarise_totals %>% 
  filter(vacc_per_week == 0.05,
         t_d3 == 180) %>%
  select(target_pop, strategy_name, deaths_med, inc_med)
x
(1842-798)/1842

y <- df_summarise %>%
  filter(date > as.Date("2021-01-01")) %>%
  filter(vacc_per_week == 0.05,
         t_d3 == 180) %>%
  group_by(target_pop, strategy_name) %>%
  summarise(deaths = max(deaths_t))
y

################################

# plot deaths - delta scenario
p_deaths_delta <- ggplot(data = filter(df_summarise,
                                 strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"),
                                 max_Rt == 5,
                                 t_d3 == 180)
                   , aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_3)) +
  scale_fill_manual(values = c("grey20", col_set_3)) +
  labs(x = "Time", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

p_deaths_delta

# plot infections - delta scenario
p_infections_delta <- ggplot(data = filter(df_summarise,
                                     max_Rt ==5,
                                     t_d3 == 180,
                                     strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"),
                                     timestep < max(df_summarise$timestep))
                       , aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_3)) +
  scale_fill_manual(values = c("grey20", col_set_3)) +
  labs(x = "Time", y = "Daily incidence per million", col = "Dose scenario", fill = "Dose scenario")

p_infections_delta

# plot deaths - omicron scenario
name <- "rq1_hic_abmodel_omicron"
df_summarise_om <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals_om <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

m <- unique(df_summarise_om$strategy_name)
m

df_summarise_om <- df_summarise_om %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))

df_summarise_totals_om <- df_summarise_totals_om %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))

df_summarise_om <- df_summarise_om %>%
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("6 months (default)", "3 months")))

df_summarise_totals_om <- df_summarise_totals_om %>%
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("6 months (default)", "3 months")))

df_summarise_om <- df_summarise_om %>%
  mutate(vfr_lab = factor(vfr, levels = c(2,4,8,16), labels = c("VFR = 2", "VFR = 4", "VFR = 8", "VFR = 16"), ordered = TRUE))

p_deaths_omicron <- ggplot(data = filter(df_summarise_om,
                                 strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"),
                                 max_Rt == 5,
                                 t_d3 == 180)
                   , aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~vfr_lab, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_3)) +
  scale_fill_manual(values = c("grey20", col_set_3)) +
  labs(x = "Time", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

p_deaths_omicron

p_inc_omicron <- ggplot(data = filter(df_summarise_om,
                                         strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"),
                                         max_Rt == 5,
                                         t_d3 == 180,
                                          timestep < max(df_summarise$timestep))
                           , aes(x = as.Date(date), y = (inc_t/target_pop*1e6), col = strategy_name)) +
   geom_ribbon(aes(ymin =inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~vfr_lab, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_3)) +
  scale_fill_manual(values = c("grey20", col_set_3)) +
  #scale_y_continuous(trans = "log10") +
  labs(x = "Time", y = "Daily incidence per million", col = "Dose scenario", fill = "Dose scenario")

p_inc_omicron

y1 <- df_summarise_om %>%
  filter(date > as.Date("2021-01-01")) %>%
  filter(vacc_per_week == 0.05,
         t_d3 == 180, vfr == 4) %>%
  group_by(target_pop, strategy_name, vfr) %>%
  summarise(deaths = max(deaths_t))
y1

y2 <- df_summarise_om %>%
  filter(date > as.Date("2021-01-01")) %>%
  filter(vacc_per_week == 0.05,
         t_d3 == 90, vfr == 4) %>%
  group_by(target_pop, strategy_name, vfr) %>%
  summarise(deaths = max(deaths_t))
y2
#########################
# summary barchart
df_om <- df_summarise_totals_om %>%
  select(deaths_med, inc_med, vfr, strategy_name, t_d3) %>%
  mutate(scenario = paste0("Omicron: \nVFR = ", vfr))
df_delta <- df_summarise_totals %>%
  filter(vacc_per_week == 0.05,
         t_d3 == 180) %>%
  select(deaths_med, inc_med, strategy_name, t_d3) %>%
    mutate(vfr = 1,
           scenario = "Delta")
vfr <- c(2,4,8,16)
df_barchart <- rbind(df_delta, df_om)
df_barchart$scenario <- factor(df_barchart$scenario, levels = c("Delta", paste0("Omicron: \nVFR = ", vfr)), ordered = TRUE)

# barplot summary of deaths
p_deaths_summary <- ggplot(data = filter(df_barchart, t_d3 == 180), aes(x = scenario, y = deaths_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Variant scenario: 6 months boost", y = "Total deaths per million", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_deaths_summary

p_deaths_summary_90 <- ggplot(data = filter(df_barchart, t_d3 == 90), aes(x = scenario, y = deaths_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Variant scenario: 3 months boost", y = "Total deaths per million", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_deaths_summary_90

# barplot summary of incidence
p_inc_summary_180 <- ggplot(data = filter(df_barchart, t_d3 == 180), aes(x = scenario, y = inc_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Variant scenario: booster 6 months", y = "Total incidence per million", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_inc_summary_180

# barplot summary of incidence
p_inc_summary_90 <- ggplot(data = filter(df_barchart, t_d3 == 90), aes(x = scenario, y = inc_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Variant scenario: booster 3 months", y = "Total incidence per million", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_inc_summary_90

# plot total doses over time
fig_doses_time <- ggplot(data = filter(df_summarise_om,
                                       strategy_name != "Pre-vaccine introduction",
                                       max_Rt == 5,
                                       vfr == 4), aes(x = as.Date(date), y  = vaccines_t/target_pop, col = strategy_name, linetype = dose_3_timing)) +
  geom_line(size = 1) +
  lims(x = c(as.Date("2021-01-01"), as.Date("2022-06-30"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Cumulative doses delivered per person", col = "Dose scenario", linetype = "Booster dose timing") +
  scale_color_manual(values = col_set)

fig_doses_time
ggsave("plots/rq1_fig_doses_time.png", fig_doses_time, height = 4, width = 7)


############################
library(patchwork)
layout <- "
AB
CC
DE
"
combined <- p_deaths_delta + age_plot + p_deaths_omicron + p_deaths_summary + heatmap_plot  +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 3, design = layout)

combined
ggsave("plots/fig2.png",combined, height = 8, width = 11)

layout2 <- "
AB
CC
"
inc_combined <- p_inc_summary_180 + p_inc_summary_90 + p_inc_omicron +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 2, design = layout2)
inc_combined
ggsave("plots/fig2_incidence.png",inc_combined, height = 6, width = 11)

boost_delay_combined <- p_deaths_summary + p_deaths_summary_90 +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") 
boost_delay_combined
ggsave("plots/fig2_boost_delay_deaths.png",boost_delay_combined, height = 4, width = 11)

#############################
# check growth rate
ss <- df_summarise_om %>%
  filter(date >= as.Date("2021-11-27"),
         date <= as.Date("2021-12-31"),
         t_d3 == 180) %>%
  select(vfr, inc_t, strategy_name, date) %>%
  group_by(vfr, strategy_name) %>%
  mutate(diff_inc_t = lead(inc_t) - inc_t,
         growth_rate = diff_inc_t / inc_t)

ggplot(data = ss, aes(x = as.Date(date), y = growth_rate, col = strategy_name)) +
  geom_line() +
  facet_wrap(~vfr)
