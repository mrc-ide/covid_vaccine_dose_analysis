d1 <- readRDS("processed_outputs/df_counter_scenario1.rds")
d2 <- readRDS("processed_outputs/df_counter_scenario5.rds")
df <- rbind(d1, d2) %>%
  mutate(income_group = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC")))

# counterfactual over time
g1 <- ggplot(data = df, aes(x = as.Date(date), y = deaths_t/target_pop * 1e6)) +
  geom_line() +
  facet_wrap(~income_group, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "date", y = "deaths per day per million", ) +
  ggtitle("Counterfactual epidemic")
g1

# show recovered over time
g2 <- ggplot(data = df, aes(x = as.Date(date), y = prop_R)) +
  facet_wrap(~income_group, nrow = 1) +
  geom_line()+
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "date", y = "% in recovered class")+
  ggtitle("Proportion in Recovered")
g2

# composite plot
library(patchwork)
plots_combined <- g1 / g2 
plots_combined

ggsave("plots/figure_s1.png", height = 7, width = 12)


# show Rt over time
g4 <- ggplot(data = filter(df, income_group == "HIC"), aes(x = as.Date(date), y = Rt)) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "date", y = "Rt") +
  ggtitle("Reproduction number")
g4
ggsave("plots/reproduction_number.png", height = 6, width = 6)
