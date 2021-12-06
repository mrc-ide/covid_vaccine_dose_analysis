name <- "rq1_hic_abmodel"
df1 <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(vaccine_doses == 2,
         t_d3 == 180) %>%
  select(date, Rt) %>%
  mutate(setting = "Substantial prior transmission",
         variant = "Delta")

name <- "rq1_hic_abmodel_omicron"
df1o <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(vaccine_doses == 2,
         t_d3 == 180,
         vfr == 4) %>%
  select(date, Rt) %>%
  mutate(setting = "Substantial prior transmission",
         variant = "Omicron")

name <- "rq3_hic_abmodel_zerocovid"
df2 <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(vaccine_doses == 2,
         t_d3 == 180,
         Rt_lift_t == "Sept '21 lift") %>%
  select(date, Rt) %>%
  mutate(setting = "Minimal prior transmission",
         variant= "Delta")

name <- "rq3_hic_abmodel_zerocovid_omicron"
df2o <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(vaccine_doses == 2,
         t_d3 == 180,
         Rt_lift_t == "Sept '21 lift") %>%
  select(date, Rt) %>%
  mutate(setting = "Minimal prior transmission",
         variant = "Omicron")

df3 <- rbind(df1, df1o, df2, df2o)%>%
  mutate(setting = factor(setting, levels = c("Substantial prior transmission", "Minimal prior transmission")))

p_Rt <- ggplot(data = df3, aes(x = as.Date(date), y = Rt, col = variant)) +
  geom_line(size = 0.8) +
  facet_wrap(~setting) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "bottom") +
  scale_color_manual(values = c("grey20", "seagreen")) +
  labs(x = "Time", y = "Rt", col = "")

p_Rt

ggsave("plots/p_Rt.png", p_Rt, height = 3, width = 8)
