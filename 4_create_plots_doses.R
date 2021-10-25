df1 <- filter(df_summarise, max_coverage == 0.8, income_group == "HIC") %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(5,9), labels = c("60+ years", "40+ years")))

# plot outputs, vaccine doses
df1_vacc <- df1 %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose")

ggplot(data = df1_vacc, aes(x = timestep, y = value/target_pop*100, col = dose)) +
  geom_line(size = 0.8) +
  facet_grid(as.factor(vaccine_doses) ~ age_groups_covered) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Timestep (days)", y = "Vaccinated (%)", col = "Dose number") +
  ggtitle("High income setting") +
  annotate(geom="text", x=500, y=25, xend=Inf, yend=Inf, label='DRAFT', color='grey', angle=45, fontface='bold', size=15, alpha=0.2, family='Arial')

ggsave("plots/Fig3_HIC_vaccinated_update.png", height = 5, width = 10)

####################################################################
df1 <- filter(df_summarise, max_coverage == 0.8, income_group == "LMIC") %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Dose 3" = "dose3_t") %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(2, 3), labels = c("2 doses", "3 doses"))) %>%
  mutate(age_groups_covered = factor(age_groups_covered, levels = c(5,9), labels = c("60+ years", "40+ years")))

# plot outputs, vaccine doses
df1_vacc <- df1 %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Dose 3"), names_to = "dose")

ggplot(data = df1_vacc, aes(x = timestep, y = value/target_pop*100, col = dose)) +
  geom_line(size = 0.8) +
  facet_grid(as.factor(vaccine_doses) ~ age_groups_covered) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Timestep (days)", y = "Vaccinated (%)", col = "Dose number") +
  ggtitle("Lower-middle income setting") +
  annotate(geom="text", x=500, y=15, xend=Inf, yend=Inf, label='DRAFT', color='grey', angle=45, fontface='bold', size=15, alpha=0.2, family='Arial')

ggsave("plots/Fig3_LMIC_vaccinated_update.png", height = 5, width = 6)
