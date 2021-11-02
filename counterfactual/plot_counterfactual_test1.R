df <- readRDS("output_counterfactual_test1/scenario_1.rds") %>%
  unnest(cols)
head(df)

p1 <- ggplot(data = df, aes(x = timestep + as.Date("2020-02-01"), y = D_count)) +
  geom_line() +
  labs(title = "Ab titre model for immune")

p1


df <- readRDS("output_counterfactual_test1/scenario_2.rds") %>%
  unnest(cols)
head(df)

p2 <- ggplot(data = df, aes(x = timestep + as.Date("2020-02-01"), y = D_count)) +
  geom_line() +
  labs(title = "Recovered class for immune")

p2

p3 <- ggplot(data = df, aes(x = timestep + as.Date("2020-02-01"), y = Rt)) +
  geom_line() +
  labs(title = "Reproduction number")

p3

library(patchwork)
combined <- p3 + p2 + p1 + plot_layout(nrow = 3)
combined
ggsave("plots/immune_model.png", height = 8, width = 5)
