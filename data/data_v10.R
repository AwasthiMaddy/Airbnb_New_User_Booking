############### load required libraries ##################
suppressMessages(require(readr))
suppressMessages(require(dplyr))
suppressMessages(require(stringr))
suppressMessages(require(lubridate))
suppressMessages(require(car))
suppressMessages(require(tm))
suppressMessages(require(Matrix))

options(dplyr.print_max = Inf)
options(dplyr.width = Inf)

############ set working directory #############
setwd("F:/Kaggle/Airbnb")


########### load raw data #############
train <- read_csv("./RawData/train_users_2.csv")
test <- read_csv("./RawData/test_users.csv")
countries <- read_csv("./RawData/countries.csv")
sessions <- read_csv("./RawData/sessions.csv")
details <- read_csv("./RawData/age_gender_bkts.csv")

########## merge train and test ###########
test$country_destination <- NA
train$trainFlag <- 1
test$trainFlag <- 0

alldata <- rbind(train, test)

########## feature engineering on user details ###########
alldata$fac_year <- substring(alldata$timestamp_first_active, 1, 4)
alldata$fac_month <- substring(alldata$timestamp_first_active, 5, 6)
alldata$fac_day <- substring(alldata$timestamp_first_active, 7, 8)
alldata$fac_date <- as.Date(paste(alldata$fac_year, alldata$fac_month, alldata$fac_day, sep = "-"))
alldata$fac_year <- as.integer(alldata$fac_year)
alldata$fac_month <- as.integer(alldata$fac_month)
alldata$fac_day <- as.integer(alldata$fac_day)
alldata$dac_year <- year(alldata$date_account_created)
alldata$dac_month <- month(alldata$date_account_created)
alldata$dac_mday <- mday(alldata$date_account_created)
alldata$dac_wday <- wday(alldata$date_account_created)

alldata$diffdate1 <- as.integer(alldata$fac_date - alldata$date_account_created)

alldata$int_1 <- ifelse(alldata$language != "en", 1, 0)
alldata$age[alldata$age < 14 | alldata$age > 100] <- -1
alldata$gender <- as.integer(as.factor(alldata$gender))
alldata$signup_method <- as.integer(as.factor(alldata$signup_method))
alldata$language <- as.integer(as.factor(alldata$language))
alldata$affiliate_channel <- as.integer(as.factor(alldata$affiliate_channel))
alldata$affiliate_provider <- as.integer(as.factor(alldata$affiliate_provider))
alldata$first_affiliate_tracked <- as.integer(as.factor(alldata$first_affiliate_tracked))
alldata$signup_app <- as.integer(as.factor(alldata$signup_app))
alldata$first_device_type <- as.integer(as.factor(alldata$first_device_type))
alldata$first_browser <- as.integer(as.factor(alldata$first_browser))

alldata$date_first_booking <- NULL

########### process user sessions #############

sessions$sessionFlag <- 1
a <- sessions[!duplicated(sessions$user_id),]
alldata <- merge(alldata, a[, c("user_id", "sessionFlag")], by.x = "id", by.y = "user_id", all.x = TRUE)
alldata$sessionFlag[is.na(alldata$sessionFlag)] <- 0

sessions <- sessions[!is.na(sessions$user_id),]
sessions$uid <- as.integer(as.factor(sessions$user_id))
sessions <- sessions %>% group_by(uid) %>% mutate(sessionLength = n())
sessions <- sessions %>% group_by(uid) %>% mutate(totalSecsElapsed = sum(secs_elapsed, na.rm = TRUE))
sessions <- sessions %>% group_by(uid) %>% mutate(distinctActions = length(unique(action)))
sessions <- sessions %>% group_by(uid) %>% mutate(distinctActionTypes = length(unique(action_type)))

sessions$action <- str_replace_all(sessions$action, "_", "")
sessions$action[is.na(sessions$action)] <- "NotKnown"
sessions$action_detail[is.na(sessions$action_detail)] <- "Unknown"
actionSummary <- sessions %>% group_by(user_id) %>% summarise(consAction = paste(action, collapse = " "))
actionDetailSummary <- sessions %>% group_by(user_id) %>% summarize(consActionDet = paste(action_detail, collapse = " "))
sessions$action_type[is.na(sessions$action_type)] <- "Unknown"
sessions$action_type[sessions$action_type == "-unknown-"] <- "Unknown"
actionTypeSummary <- sessions %>% group_by(user_id) %>% summarize(consActionType = paste(action_type, collapse = " "))
sessions <- sessions[!duplicated(sessions$user_id),]


alldata <- merge(alldata, sessions[, c("user_id", "sessionLength", "totalSecsElapsed", "distinctActions", "distinctActionTypes")], by.x = "id", by.y = "user_id", all.x = TRUE)
alldata <- merge(alldata, actionSummary, by.x = "id", by.y = "user_id", all.x = TRUE)  # merge the session and user data
alldata <- merge(alldata, actionDetailSummary, by.x = "id", by.y = "user_id", all.x = TRUE)  # merge the session and user data
alldata <- merge(alldata, actionTypeSummary, by.x = "id", by.y = "user_id", all.x = TRUE)
alldata$consAction[is.na(alldata$consAction)] <- "noSessions"
alldata$consActionDet[is.na(alldata$consActionDet)] <- "noSessions"
alldata$consActionType[is.na(alldata$consActionType)] <- "noSessions"
alldata[is.na(alldata)] <- -1
alldata$cons <- paste(alldata$consAction, alldata$consActionDet, alldata$consActionType, sep = " ")
alldata$consAction <- NULL
alldata$consActionDet <- NULL
alldata$consActionType <- NULL

alldata <- alldata[order(-alldata$trainFlag),] # order the data in train -> test manner for easy splitting

alldata <- alldata[!(alldata$sessionFlag == 0 & alldata$trainFlag == 1),]

target <- recode(alldata[alldata$trainFlag == 1,]$country_destination,
                 "'NDF'=0; 
                 'US'=1; 
                 'other'=2; 
                 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")

testID <- alldata[alldata$trainFlag == 0,]$id
tid <- alldata[alldata$trainFlag == 0 & alldata$sessionFlag == 0,]$id

cp <- Corpus(VectorSource(alldata$cons))
dtm <- DocumentTermMatrix(cp, control = list(weighting = weightTfIdf))
# dtm_matrix <-   sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
#                              dims=c(dtm$nrow, dtm$ncol))

reduced_dtm <- removeSparseTerms(dtm, sparse = 0.9998)
reduced_dtm_matrix <-   sparseMatrix(i=reduced_dtm$i, j=reduced_dtm$j, x=reduced_dtm$v,
                                     dims=c(reduced_dtm$nrow, reduced_dtm$ncol))
trainC <- 73815
testC <- 73815 + nrow(test)

denseFeatures <- names(alldata)[!names(alldata) %in% c("id", "date_account_created", "timestamp_first_active", "country_destination", "trainFlag",
                                                       "fac_date", "consActionDet", "consAction", "cons","sessionFlag")]


trainD <- cBind(Matrix(as.matrix(alldata[, denseFeatures][1:trainC,]), sparse = TRUE), reduced_dtm_matrix[1:trainC,])
testD <- cBind(Matrix(as.matrix(alldata[, denseFeatures][(trainC+1) : testC,]), sparse = TRUE), reduced_dtm_matrix[(trainC+1): testC,])

save(trainD, file = "./data_V10/trainD.RData")
save(testD, file  = "./data_V10/testD.RData")
target <- target[1:trainC]
labels <- data.frame(country_destination = target)
write_csv(labels, "./data_V10/labels.csv")
ids <- data.frame(id = testID)
write_csv(ids, "./data_V10/test_user_ids.csv")
tID <- data.frame(tid = tid)
write_csv(tID, "./data_V10/tid.csv")
