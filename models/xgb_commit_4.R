require(readr)
require(xgboost)

setwd("F:/Kaggle/Airbnb")
source("./scripts/source.R")
set.seed(12)

load("./data_V4/trainD.RData")
load("./data_V4/testD.RData")
labels <- read_csv("./data_V4/labels.csv")
testId <- read_csv("./data_V4/test_user_ids.csv")
target <- labels$country_destination


param  <- list(objective = "multi:softprob",
               num_class = 12,
               max_depth = 6,
               eta = 0.01,
               subsample = 0.78,
               colsample_bytree = 0.4,
               min_child_weight = 12,
               eval_metric = "mlogloss")

dtrain <- xgb.DMatrix(data = trainD, label = target)
watchlist <- list(train = dtrain)

clf <- xgb.train(params = param,
                 data = dtrain,
                 nround = 2150,
                 print.every.n = 20,
                 watchlist = watchlist)

pred <- predict(clf, testD)
pmat <- reshape_probs(pred, testId)
pred_hc <- topChoices(pmat)
sub <- createSubmission(pred_hc)


write_csv(sub, "./Submissions/xgb_commit_4.csv")
write_csv(pmat, "./Raw Predictions/xgb_commit_4_raw.csv")
