library(ggplot2)

gbm_df <- results |> filter(Model == "GBM")
ggplot(gbm_df, aes(x = n_estimators, y = RMSE, color = factor(learning_rate), group = factor(learning_rate))) +
  geom_line() + geom_point() +
  labs(title = "GBM: RMSE от ntrees при разных learning_rate, depth=3",
       color = "learning_rate", x = "ntrees", y = "RMSE") +
  theme_minimal()

library(plotmo)
plot_gbm(model_gbm_fun, ylim=NULL)

gbm_df <- results |> filter(Model == "GBM_depth")
gbm_df <- gbm_df |> mutate(
     depth = str_extract(Param1, "\\d+$")
)
ggplot(gbm_df, aes(x = n_estimators, y = RMSE, color = depth, group = depth)) +
  geom_line() + geom_point() +
  labs(title = "GBM: RMSE от ntrees и глубины при learning_rate=0.01",
       color = "depth", x = "ntrees", y = "RMSE") +
  theme_minimal()