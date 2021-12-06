##############################################
# AGE PLOT
name <- "rq1_hic_abmodel_age"
df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))

x <- seq(0,80,5)
y <- seq(4,84,5)
z <- paste0(x,"-",y)
z[17] <- "80+"

age_plot <- ggplot(data = summary_df, aes(x = age, y= value_med, fill =strategy_name)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0),
        legend.position = "none") +
  labs(x = "Age", y = "Total deaths since vaccine start", fill = "Dose scenario") +
  scale_x_continuous(breaks = 1:17, labels = z) +
  scale_fill_manual(values = col_set)

age_plot
##############################################
# HEATMAP
name <- "rq1_hic_omicron_heatmap"
df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))

heatmap_plot <- ggplot(data = filter(df_summarise, vfr <= 8), aes(x = factor(max_Rt_omicron), y = factor(vfr), fill = deaths_med)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Rt_max", y = "Variant fold reduction", fill = "Deaths since\nvaccination") 
heatmap_plot

##############################################
################################
# set up the colours!
cc <- scales::viridis_pal(end = 0.9)(6)
col_set <- c(cc[1], cc[3], cc[4], cc[5], cc[6])
col_set_3 <- c(col_set[1], col_set[2], col_set[5])

col_set_spacing <- scales::viridis_pal(option = "B", begin = 0.4, end = 0.8)(2)

# some factors need reordering
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
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("Pre-vaccine introduction", "6 months (default)", "8 months", "12 months")))

df_summarise_totals <- df_summarise_totals %>%
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("6 months (default)", "8 months", "12 months")))

################################

# plot total doses over time
fig_doses_time <- ggplot(data = filter(df_summarise,
                                       strategy_name != "Pre-vaccine introduction",
                                       max_Rt == 5,
                                       t_d3 == 180), aes(x = as.Date(date), y  = vaccines_t/target_pop, col = strategy_name)) +
  geom_line(size = 1) +
  lims(x = c(as.Date("2020-01-01"), as.Date("2023-06-30"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Cumulative doses delivered per person", col = "Dose scenario") +
  scale_color_manual(values = col_set)

fig_doses_time
ggsave("plots/rq1_fig_doses_time.png", fig_doses_time, height = 4, width = 7)

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
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("Pre-vaccine introduction", "6 months (default)", "8 months", "12 months")))

df_summarise_totals_om <- df_summarise_totals_om %>%
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("6 months (default)", "8 months", "12 months")))

df_summarise_om <- df_summarise_om %>%
  mutate(vfr_lab = factor(vfr, levels = c(2,4,8), labels = c("VFR = 2", "VFR = 4", "VFR = 8")))


p_deaths_omicron <- ggplot(data = filter(df_summarise_om,
                                 strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"),
                                 max_Rt == 5,
                                 t_d3 == 180)
                   , aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~vfr_lab) +
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
                           , aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~vfr_lab) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_3)) +
  scale_fill_manual(values = c("grey20", col_set_3)) +
  labs(x = "Time", y = "Daily incidence per million", col = "Dose scenario", fill = "Dose scenario")

p_inc_omicron

#########################
# summary barchart
df_om <- df_summarise_totals_om %>%
  select(deaths_med, inc_med, vfr, strategy_name) %>%
  mutate(scenario = paste0("Omicron: \nVFR = ", vfr))
df_delta <- df_summarise_totals %>%
  filter(vacc_per_week == 0.05,
         t_d3 == 180) %>%
  select(deaths_med, inc_med, strategy_name) %>%
    mutate(vfr = 1,
           scenario = "Delta")

df_barchart <- rbind(df_delta, df_om)

# barplot summary of deaths
p_deaths_summary <- ggplot(data = df_barchart, aes(x = scenario, y = deaths_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Variant scenario", y = "Deaths per mill since vacc start", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_deaths_summary

# barplot summary of incidence
p_inc_summary <- ggplot(data = df_barchart, aes(x = scenario, y = inc_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Variant scenario", y = "Deaths per mill since vacc start", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_inc_summary

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
inc_combined <- p_infections_delta + p_inc_summary + p_inc_omicron+
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 2, design = layout2)
inc_combined
ggsave("plots/fig2_incidence.png",inc_combined, height = 6, width = 11)
