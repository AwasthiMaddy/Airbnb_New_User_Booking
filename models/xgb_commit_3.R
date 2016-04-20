require(readr)
require(xgboost)

setwd("F:/Kaggle/Airbnb")
source("./scripts/source.R")
set.seed(12)

load("./Data/trainD.RData")
load("./Data/testD.RData")
labels <- read_csv("./Data/labels.csv")
testId <- read_csv("./Data/test_user_ids.csv")
target <- labels$country_destination


param  <- list(objective = "multi:softprob",
               num_class = 12,
               max_depth = 6,
               eta = 0.02,
               subsample = 0.85,
               colsample_bytree = 0.7,
               min_child_weight = 3,
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


write_csv(sub, "./Submissions/xgb_commit_3.csv")
write_csv(pmat, "./Raw Predictions/xgb_commit_3_raw.csv")
