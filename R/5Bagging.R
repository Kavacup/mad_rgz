library(ggplot2)

bag_df <- results |> filter(Model == "Bagging")
ggplot(bag_df, aes(x = n_estimators, y = RMSE)) +
  geom_line() + geom_point() +
  labs(title = "Bagging: RMSE от n.trees",
       x = "n.trees", y = "RMSE") +
  theme_minimal()
