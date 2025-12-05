# install.packages(c("tidyverse","caret","corrplot","GGally","randomForest","ranger","ipred","gbm","data.table"))

library(tidyverse)
library(caret)
library(corrplot)
library(GGally)
library(randomForest)
library(ranger)
library(ipred)
library(gbm)
library(data.table)

set.seed(42)
train <- read_csv("datasets/california_train.csv")
test <- read_csv("datasets/california_test.csv")

# df <- read_csv("datasets/dataset_prepared.csv")
# in_train <- createDataPartition(df$Median_Income, p=0.8, list=FALSE)
# train <- df[in_train, ]
# test  <- df[-in_train, ]

# только числовые фичи
numeric_df <- train |> select_if(is.numeric)
corr_mat <- cor(numeric_df, use = "pairwise.complete.obs", method = "pearson")
corrplot(corr_mat,
  method = "color", type = "upper", tl.cex = 0.7,
  tl.col = "black", addCoef.col = "black",
  number.cex = 0.6,
  col = colorRampPalette(c("blue", "white", "red"))(200)
)

target <- "Median_Income"
corr_with_target <- sort(abs(corr_mat[, target]), decreasing = TRUE)
corr_with_target
