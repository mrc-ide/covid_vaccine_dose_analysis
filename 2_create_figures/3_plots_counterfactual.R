name <- "counterfactual_abmodel"

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

df_summarise <- df_summarise %>%
  mutate(date = as.Date(timestep + as.Date("2020-02-01")))

ggplot(data = df_summarise, aes(x = as.Date(date), y = deaths_t, col = factor(mu_ab_infection))) +
  geom_line()

