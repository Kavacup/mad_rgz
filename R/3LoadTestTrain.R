target_name <- "Median_Income"
x_train <- train |> select(-all_of(target_name))
y_train <- train |> pull(target_name)

x_test  <- test |> select(-all_of(target_name))
y_test  <- test |> pull(target_name)
