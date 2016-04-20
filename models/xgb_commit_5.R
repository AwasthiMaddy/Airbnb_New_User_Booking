require(readr)
require(xgboost)

setwd("F:/Kaggle/Airbnb")
source("./scripts/source.R")
set.seed(12)

load("./data_V5/trainD.RData")
load("./data_V5/testD.RData")
labels <- read_csv("./data_V5/labels.csv")
testId <- read_csv("./data_V5/test_user_ids.csv")
target <- labels$country_destination


param  <- list(objective = "multi:softprob",
               num_class = 12,
               max_depth = 6,
               eta = 0.015,
               subsample = 0.9,
               colsample_bytree = 0.6,
               min_child_weight = 30,
               alpha = 4,
               eval_metric = "mlogloss")

dtrain <- xgb.DMatrix(data = trainD, label = target)
watchlist <- list(train = dtrain)

clf <- xgb.train(params = param,
                 data = dtrain,
                 nround = 1600,
                 print.every.n = 20,
                 watchlist = watchlist)

pred <- predict(clf, testD)
pmat <- reshape_probs(pred, testId)
pred_hc <- topChoices(pmat)
sub <- createSubmission(pred_hc)

write_csv(sub, "./Submissions/xgb_commit_5.csv")
write_csv(pmat, "./Raw Predictions/xgb_commit_5_raw.csv")
