rmse <- function(actual, pred) { sqrt(mean((actual - pred)^2, na.rm = TRUE)) }
r2   <- function(actual, pred)  { cor(actual, pred)^2 }

# Результирующий датафрейм
results <- data.frame(Model=character(), Param1=character(), Param2=character(),
                      n_estimators=integer(), learning_rate=numeric(),
                      RMSE=numeric(), R2=numeric(), stringsAsFactors = FALSE)

# Для bagging в ipred аргумент nbagg контролирует число бэггинг-итераций
nbagg_list <- c(10, 50, 100, 200, 500, 1000)

for (nbagg in nbagg_list) {
  model_bag <- ipred::bagging(as.formula(paste(target_name, "~ .")),
                              data = train,
                              nbagg = nbagg
                              )
  preds <- predict(model_bag, newdata = test)
  results <- rbind(results, data.frame(Model="Bagging", Param1=NA, Param2=NA,
                                       n_estimators = nbagg, learning_rate = NA,
                                       RMSE = rmse(y_test, preds),
                                       R2   = r2(y_test, preds)))
}

# доля m число n
ntree_list <- c(10, 50, 100, 200)
mtry_list  <- c(ncol(x_train) * 0.2, ncol(x_train) * 0.4, ncol(x_train) * 0.6, ncol(x_train) * 0.8, ncol(x_train))

for (nt in ntree_list) {
  for (m in unique(mtry_list)) {
    model_rf <- randomForest::randomForest(x = x_train, y = y_train, 
                                           ntree = nt, mtry = m)
    preds <- predict(model_rf, newdata = x_test)
    results <- rbind(results, data.frame(Model="RandomForest_n_mtry",
                                         Param1 = paste0("mtry=", m),
                                         Param2 = NA,
                                         n_estimators = nt,
                                         learning_rate = NA,
                                         RMSE = rmse(y_test, preds),
                                         R2   = r2(y_test, preds)))
  }
}

# глубина, доля m
maxnodes_list <- c(3, 5, 10, 15, 30)
mtry_list  <- c(ncol(x_train) * 0.2, ncol(x_train) * 0.4, ncol(x_train) * 0.6, ncol(x_train) * 0.8, ncol(x_train))

for (mn in maxnodes_list) {
  for (m in unique(mtry_list)) {
    model_rf <- randomForest::randomForest(x = x_train, y = y_train, 
                                           ntree = 100, mtry = ncol(x_train),
                                           maxnodes = mn)
    preds <- predict(model_rf, newdata = x_test)
    results <- rbind(results, data.frame(Model="RandomForest_mtry_maxnodes",
                                         Param1 = paste0("mtry=", m),
                                         Param2= paste0("maxnodes=", mn),
                                         n_estimators = 100,
                                         learning_rate = NA,
                                         RMSE = rmse(y_test, preds),
                                         R2   = r2(y_test, preds)))
  }
}

# глубина, число n
maxnodes_list <- c(3, 5, 10, 15, 30)
ntree_list <- c(10, 50, 100, 200)

for (nt in ntree_list) {
  for (mn in maxnodes_list) {
    model_rf <- randomForest::randomForest(x = x_train, y = y_train, 
                                           ntree = nt, mtry = ncol(x_train),
                                           maxnodes = mn)
    preds <- predict(model_rf, newdata = x_test)
    results <- rbind(results, data.frame(Model="RandomForest_n_maxnodes",
                                         Param1 = paste0("mtry=", ncol(x_train)),
                                         Param2= paste0("maxnodes=", mn),
                                         n_estimators = nt,
                                         learning_rate = NA,
                                         RMSE = rmse(y_test, preds),
                                         R2   = r2(y_test, preds)))
  }
}


# gbm
lr_list <- c(0.001, 0.01, 0.1, 0.3)
trees_list <- c(10, 50, 100, 200, 500, 1000)

for (lr in lr_list) {
  for (nt in trees_list) {
    model_gbm <- gbm::gbm(formula = as.formula(paste(target_name, "~ .")),
                         distribution = "gaussian",
                         data = train,
                         n.trees = nt,
                         shrinkage = lr,
                         interaction.depth = 3
                         )
    preds <- predict(model_gbm, newdata = test, n.trees = nt)
    results <- rbind(results, data.frame(Model="GBM",
                                         Param1 = paste0("depth=3"),
                                         Param2=NA,
                                         n_estimators = nt,
                                         learning_rate = lr,
                                         RMSE = rmse(y_test, preds),
                                         R2   = r2(y_test, preds)))
  }
}

# GBM с большим числом деревьев и малым learning rate
model_gbm_fun <- gbm::gbm(formula = as.formula(paste(target_name, "~ .")),
                         distribution = "gaussian",
                         data = train,
                         n.trees = 50000,
                         shrinkage = 0.001,
                         interaction.depth = 3
                         )
preds <- predict(model_gbm_fun, newdata = test, n.trees = 50000)
results <- rbind(results, data.frame(Model="GBM_fun",
                                     Param1 = paste0("depth=3"),
                                     Param2=NA,
                                     n_estimators = 50000,
                                     learning_rate = 0.001,
                                     RMSE = rmse(y_test, preds),
                                     R2   = r2(y_test, preds)))


depth_list <- c(1, 2, 3, 4)
trees_list <- c(10, 50, 100, 200)

for (d in depth_list) {
  for (nt in trees_list) {
    model_gbm <- gbm::gbm(formula = as.formula(paste(target_name, "~ .")),
                         distribution = "gaussian",
                         data = train,
                         n.trees = nt,
                         shrinkage = 0.01,
                         interaction.depth = d
                         )
    preds <- predict(model_gbm, newdata = test, n.trees = nt)
    results <- rbind(results, data.frame(Model="GBM_depth",
                                         Param1 = paste0("depth=",d),
                                         Param2=NA,
                                         n_estimators = nt,
                                         learning_rate = 0.01,
                                         RMSE = rmse(y_test, preds),
                                         R2   = r2(y_test, preds)))
  }
}

model_rf_optimal <- randomForest::randomForest(x = x_train, y = y_train, 
                                       ntree = 500, mtry = ncol(x_train) * 0.4)
preds <- predict(model_rf_optimal, newdata = x_test)
results <- rbind(results, data.frame(Model="RandomForest_optimal",
                                     Param1 = paste0("mtry=", ncol(x_train) * 0.4),
                                     Param2=NA,
                                     n_estimators = 500,
                                     learning_rate = NA,
                                     RMSE = rmse(y_test, preds),
                                     R2   = r2(y_test, preds)))

# Просмотр таблицы результатов
results |> arrange(RMSE) |> head(10)
