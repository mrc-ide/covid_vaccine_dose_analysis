name <- "rq1_hic_abmodel_omicron"

################################
# set up the colours!
cc <- scales::viridis_pal(end = 0.9)(6)
col_set <- c(cc[1], cc[3], cc[4], cc[5], cc[6])
col_set_3 <- c(col_set[1], col_set[2], col_set[5])

col_set_spacing <- scales::viridis_pal(option = "B", begin = 0.4, end = 0.8)(2)

################################
# some factors need reordering
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

df_summarise <- df_summarise %>%
  mutate(vfr_lab = factor(vfr, levels = c(2,4,8), labels = c("VFR = 2", "VFR = 4", "VFR = 8")))

################################

# plot total doses over time
fig_doses_time <- ggplot(data = filter(df_summarise,
                           strategy_name != "Pre-vaccine introduction"), aes(x = as.Date(date), y  = vaccines_t/target_pop, col = strategy_name)) +
  geom_line(size = 1) +
  lims(x = c(as.Date("2020-01-01"), as.Date("2023-01-01"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Cumulative doses delivered per person", col = "Dose scenario") +
  scale_color_manual(values = col_set)


fig_doses_time

# plot outputs: deaths
p_deaths <- ggplot(data = filter(df_summarise,
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

p_deaths
ggsave("plots/p_deaths_omicron_rq1.png", p_deaths, height = 3, width = 8)

# barplot summary of deaths
p_deaths_summary <- ggplot(data = filter(df_summarise_totals,
                                         t_d3 == 180), 
                           aes(x = factor(max_Rt), y = deaths_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  #geom_errorbar(aes(ymin = deaths_lower/target_pop * 1e6, ymax = deaths_upper / target_pop * 1e6), position = position_dodge()) +
  scale_fill_manual(values = col_set) +
  labs(x = "Dose scenario", y = "Total deaths per million since vaccination start", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_deaths_summary

# plot outputs: infections
p_infections <- ggplot(data = filter(df_summarise,
                           max_Rt ==5,
                           std10 == 0.44,
                           t_d3 == 180,
                           strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"),
                           timestep < max(df_summarise$timestep))
             , aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_3)) +
  scale_fill_manual(values = c("grey20", col_set_3)) +
  labs(x = "Time", y = "Daily incidence per million", col = "Dose scenario", fill = "Dose scenario")

p_infections

p_infections_summary <- ggplot(data = filter(df_summarise_totals, std10 == 0.44, t_d3 == 180), aes(x = factor(max_Rt), y = inc_med/target_pop * 1e3, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Dose scenario", y = "Tncidence per thousand since vacc start", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_infections_summary

# plot outputs: deaths with wider dose spacing
# p_spacing <- ggplot(data = filter(df_summarise,
#                            rollout_rate == "Default",
#                            std10 == 0.44,
#                            max_Rt == 5,
#                            strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, booster 60y+"))
#              , aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = dose_3_timing)) +
#   geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = dose_3_timing), alpha = 0.5, col = NA) +
#   geom_line() +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = NA),
#         panel.border = element_blank(),
#         axis.line = element_line(),
#         legend.text.align = 0) +
#   scale_fill_manual(values = c("grey20", col_set[2], col_set_spacing)) +
#   scale_color_manual(values = c("grey20",col_set[2], col_set_spacing)) +
#   labs(x = "Time", y = "Daily deaths per million", col = "Booster dose timing", fill = "Booster dose timing")
# 
# p_spacing
# 
# p_spacing_summary_deaths <- ggplot(data = filter(df_summarise_totals,
#                                           rollout_rate == "Default",
#                                           waning == "Default",
#                                           strategy_name %in% c("10y+ 2 doses, booster 60y+")), aes(x = dose_3_timing, y = deaths_med, fill = dose_3_timing)) +
#   geom_col(position = "dodge", alpha = 0.8) +
#   scale_fill_manual(values = c(col_set[2], col_set_spacing)) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = NA),
#         panel.border = element_blank(),
#         axis.line = element_line(),
#         legend.text.align = 0,
#         legend.position = "none") +
#   labs(x = "Booster dose timing", y = "Deaths per mill since vaccination")
# 
# p_spacing_summary_deaths

################################
# name <- "rq1_hic_child_abmodel"
# 
# df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
# df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))
# 
# p5 <- ggplot(data = filter(df_summarise_totals,
#                            waning == "Default",
#                            max_Rt == 3), aes(x = strategy_name, y = deaths_since_22_med/target_pop * 1e6, fill = strategy_name)) +
#   geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
#   geom_errorbar(aes(ymin = deaths_since_22_lower/target_pop * 1e6, ymax = deaths_since_22_upper / target_pop * 1e6), position = position_dodge()) +
#   scale_fill_manual(values = c(col_set[1], "burlywood4", "grey30" )) +
#   labs(x = "Dose scenario", y = "Deaths per mill since vaccination", col = "Dose scenario \n no boosters", fill = "Dose scenario \n no boosters") +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = NA),
#         panel.border = element_blank(),
#         axis.line = element_line(),
#         legend.text.align = 0,
#         aspect.ratio = 1,
#         axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0))
# p5
# 
# p6 <- ggplot(data = filter(df_summarise_totals,
#                            waning == "Default",
#                            max_Rt == 3), aes(x = strategy_name, y = inc_med/target_pop * 1e6, fill = strategy_name)) +
#   geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
#   geom_errorbar(aes(ymin = inc_lower/target_pop * 1e6, ymax = inc_upper / target_pop * 1e6), position = position_dodge()) +
#   scale_fill_manual(values = c(col_set[1], "burlywood4", "grey30" )) +
#   labs(x = "Dose scenario", y = "Cases per mill since vaccination", col = "Dose scenario \n no boosters", fill = "Dose scenario \n no boosters") +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = NA),
#         panel.border = element_blank(),
#         axis.line = element_line(),
#         legend.text.align = 0,
#         aspect.ratio = 1,
#         axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0))
# p6
# 
# p7 <- ggplot(data = filter(df_summarise,
#                            rollout_rate == "Default",
#                            waning == "Default",
#                            dose_3_timing == "6 months (default)")
#              , aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
#   geom_ribbon(aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
#   geom_line() +
#   theme_bw() +  facet_wrap(~transmission) +
#   facet_wrap(~transmission) +
#   theme(strip.background = element_rect(fill = NA),
#         panel.border = element_blank(),
#         axis.line = element_line(),
#         legend.text.align = 0) +
#   scale_color_viridis_d() +
#   scale_fill_viridis_d() +
#   labs(x = "Time", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")
# p7
# 
# # plot total doses over time
# p8 <- ggplot(data = filter(df_summarise,
#                            rollout_rate == "Default",
#                            waning == "Default",
#                            dose_3_timing == "6 months (default)",
#                            strategy_name != "Pre-vaccine introduction"), aes(x = as.Date(date), y  = vaccines_t/target_pop, col = strategy_name)) +
#   geom_line(size = 1) +
#   #facet_wrap(~t_d3, nrow = 3)+
#   lims(x = c(as.Date("2020-01-01"), as.Date("2023-01-01"))) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = NA),
#         panel.border = element_blank(),
#         axis.line = element_line(),
#         legend.text.align = 0) +
#   labs(x = "Year", y = "Cumulative doses delivered per person", col = "Dose scenario") +
#   scale_color_viridis_d(direction = -1)
# 
# p8
# #p0 <- p0 + theme(legend.position = "none")
# #p2 <- p2 + theme(legend.position = "none")
# 
# ######
# # age plot
# name <- "rq1_hic_abmodel_age"
# 
# df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
# 
# x <- seq(0,80,5)
# y <- seq(4,84,5)
# z <- paste0(x,"-",y)
# z[17] <- "80+"
# 
# age_plot <- ggplot(data = summary_df, aes(x = age, y= value_med, fill =strategy_name)) +
#   geom_col(position = "dodge", alpha = 0.8) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = NA),
#         panel.border = element_blank(),
#         axis.line = element_line(),
#         legend.text.align = 0,
#         axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0),
#         legend.position = "none") +
#   labs(x = "Age", y = "Total deaths since vaccine start", fill = "Dose scenario") +
#   scale_x_continuous(breaks = 1:17, labels = z) +
#   scale_fill_manual(values = col_set)
# 
# age_plot

library(patchwork)

layout <- "
AAABBCC
DDDEEFG
"

combined <- p_deaths + p_deaths_summary + age_plot + p_spacing + p_spacing_summary_deaths + p5 + p6 +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 6, nrow = 2, design = layout, widths = c(1,1,1,1,1,1.5,1.5))

combined

ggsave(paste0("plots/plots_impact_", name, ".png"), height = 7, width = 15)

ggsave("plots/plots_impact_rq1_hic_fig_doses_timing.png",fig_doses_time,  height = 6, width = 8)

combined_infections <- p_infections + p_infections_summary +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 1, widths = c(2,1))
combined_infections
ggsave(paste0("plots/plots_impact_", name, "_infections.png"), height = 4, width = 10)

combined_deaths <- p_deaths + p_deaths_summary +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 1, widths = c(2,1))
combined_deaths
ggsave(paste0("plots/plots_impact_", name, "_deaths.png"), height = 4, width = 10)
