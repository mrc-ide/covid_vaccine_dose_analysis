df <- readRDS("output/1_vaccine_dose_strategies.rds")
dft <- df %>% unnest(cols)

ggplot(data = dft, aes(x = timestep, y = deaths_t, col = factor(max_coverage))) +
  geom_line() +
  facet_grid(coverage~vaccine_doses) +
  theme_bw() +
  scale_color_viridis_d()


# TO DO: plot vaccine doses given out over time (similar to vignette) for each coverage strategy (check we are vaccinating people as we are supposed to)

# TO DO: calculate proportion of population in Recovered class when vaccination commences

# TO DO: calculate deaths averted for each scenario combination, some kind of bar plot...

# TO DO: can we also generate daily infections?