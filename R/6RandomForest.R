rf_df <- results |> filter(Model == "RandomForest_n_maxnodes")
rf_df <- rf_df |> mutate(
     maxnodes = as.numeric(str_extract(Param2, "\\d+$"))
)

ggplot(rf_df, aes(x = maxnodes, y = RMSE, color = as.factor(n_estimators), group = as.factor(n_estimators))) +
  geom_line() + geom_point() +
  labs(title = "RandomForest: RMSE от ntree при разной глубине, mtry=5",
       color = "ntree", x = "maxnodes", y = "RMSE") +
  theme_minimal()

rf_df <- results |> filter(Model == "RandomForest_n_mtry")
rf_df <- rf_df |> mutate(mtry = str_extract(Param1, "\\d+$"))

ggplot(rf_df, aes(x = mtry, y = RMSE, color = as.factor(n_estimators), group = as.factor(n_estimators))) +
  geom_line() + geom_point() +
  labs(title = "RandomForest: RMSE от доли при разных ntree",
       color = "ntree", x = "mtry", y = "RMSE") +
  theme_minimal()

rf_df <- results |> filter(Model == "RandomForest_mtry_maxnodes")
rf_df <- rf_df |> mutate(
     mtry = str_extract(Param1, "\\d+$"),
     maxnodes = str_extract(Param2, "\\d+$")
)

ggplot(rf_df, aes(x = mtry, y = RMSE, color = maxnodes, group = maxnodes)) +
  geom_line() + geom_point() +
  labs(title = "RandomForest: RMSE от доли при разной глубине, n=100",
       color = "maxnodes", x = "mtry", y = "RMSE") +
  theme_minimal()