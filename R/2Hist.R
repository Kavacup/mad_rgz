long_df <- numeric_df |>
  select(-contains("ID")) |>
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

ggplot(long_df, aes(x = value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~feature, scales = "free") +
  theme_minimal() +
  labs(title = "Распределения числовых признаков", x = NULL, y = "count")
