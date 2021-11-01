df <- readRDS("output_counterfactual_test1/scenario_1.rds") %>%
  unnest(cols)
head(df)

ggplot(data = df, aes(x = timestep, y = IRec_count)) +
  geom_line()


df <- readRDS("counterfactual/df.rds")
