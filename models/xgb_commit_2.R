require(readr)
require(caret)
require(xgboost)

setwd("~/Kaggle/Airbnb")
source("./scripts/source.R")
set.seed(12)

load("./Processed_data_2/trainD2.RData")
load("./Processed_data_2/testD2.RData")
labels <- read_csv("./Data/labels.csv")
testId <- read_csv("./Data/test_user_ids.csv")
target <- labels$country_destination

param <-   param <- list(objective = "multi:softprob",
                         num_class = 12,
                         max_depth = 6,
                         eta = 0.02,
                         subsample = 0.8,
                         colsample_bytree = 0.2,
                         min_child_weight = 8,
                         eval_metric = "mlogloss")

dtrain <- xgb.DMatrix(data = trainD, label = target)
watchlist <- list(train = dtrain)

clf <- xgb.train(params = param,
                 data = dtrain,
                 nround = 3000,
                 watchlist = watchlist)

pred <- predict(clf, testD)
pmat <- reshape_probs(pred, testId)
pred_hc <- topChoices(pmat)
sub <- createSubmission(pred_hc)



write_csv(sub, "./Submissions/xgb_commit_2.csv")
write_csv(pmat, "./Raw Predictions/xgb_commit_2_raw.csv")