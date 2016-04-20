
############### load required libraries ##################
suppressMessages(require(readr))
suppressMessages(require(dplyr))
suppressMessages(require(stringr))
suppressMessages(require(lubridate))
suppressMessages(require(car))
suppressMessages(require(tm))
suppressMessages(require(Matrix))


############ set working directory #############
setwd("~/Kaggle/Airbnb")


########### load raw data #############
train <- read_csv("./DataNew/train_users_2.csv")
test <- read_csv("./DataNew/test_users.csv")
countries <- read_csv("./Data/countries.csv")
sessions <- read_csv("./DataNew/sessions.csv")
details <- read_csv("./Data/age_gender_bkts.csv")

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

alldata$diffdate1 <- as.integer(alldata$fac_date - alldata$date_account_created)
alldata$weekday <- wday(alldata$date_account_created)

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

account <- c("create_user", "user_profile_content_update", "user_profile", "edit_profile", "update_user_profile", "profile_references", "profile_reviews", 
             "header_userpic", "user_profile_content_update", "user_reviews", "update_user", "user_social_connections", "user_listings", "user_friend_recommendations",
             "user_languages", "modify_users",  "view_user_real_names",  "create_phone_numbers", "phone_numbers", "set_password", "change_password", "forgot_password",
             "set_password_page", "admin_templates", "user_social_connections", "account_privacy_settings", "dashboard", "homepage", "account_notification_settings",
             "remove_dashboard_alert")

verification <- c("profile_verifications", "phone_verification_success", "view_identity_verifications", "confirm_email_link", "confirm_email", "request_new_confirm_email")
engageL1 <- c("cancellation_policies", "guest_cancellation", "cancellation_policy_click", "cancellation_policy", "payment_instruments", "account_payment_methods",
              "create_payment_instrument", "set_default_payment_instrument", "terms_and_privacy", "email_wishlist_button", "account_payout_preferences", "toggle_archived_thread")

engageL2 <- c("message_post", "message_thread", "send_message", "lookup_message_thread", "message_inbox", "account_transaction_history", "previous_trips", "popular_wishlists",
              "toggle_starred_thread", "pending", "share")

login <- c("login_page", "login", "login_modal", "oauth_login", "signup_login_page")
p4 <- c("p4",  "p4_refund_policy_terms", "p4_terms" )
search <- c("view_search_results", "view_locations", "special_offer_field")
deactivate <- c("deactivate_user_account", "delete_phone_numbers", "delete_listing", "delete_listing_description", "delete_payment_instrument" )
query <- c("similar_listings", "manage_listing", "create_listing", "your_listings", "listing_reviews_page", "update_listing", "update_listing_description", "listing_reviews",
           "user_listings", "translate_listing_reviews", "listing_recommendations", "view_listing", "listing_descriptions", "click_reviews", "move_map", "photos")

wishlist <- c("wishlist_content_update", "user_wishlists", "wishlist",  "popular_wishlists", "friends_wishlists", "airbnb_picks_wishlists", "email_wishlist",
              "email_wishlist_button", "wishlist_note")

pre_book <- c("guest_itinerary", "host_guarantee", "guest_receipt", "place_worth", "calculate_worth", "user_tax_forms","change_trip_characteristics", "view_security_checks",
              "view_resolutions", "change_availability", "move_map", "click_amenities", "trip_availability")

book <- c("booking", "book_it", "instant_book", "request_to_book", "complete_booking", "apply_coupon", "apply_coupon_error", "apply_coupon_click", "coupon_field_focus", 
          "coupon_code_click", "apply_coupon_click_success")

post_book <- c("post_checkout_action", "contact_host", "change_contact_host_dates", "host_guarantee", "host_home", "host_respond",  "message_to_host_focus", 
               "your_reservations",  "reservations", "view_reservations",  "modify_reservations", "host_refund_guest", "respond_to_alteration_request", "change_or_alter",
               "create_alteration_request", "your_trips", "alteration_field" )


sessions$summarizedAction <- ifelse(sessions$action_detail %in% account, "Account",
                                    ifelse(sessions$action_detail %in% verification, "Verification",
                                           ifelse(sessions$action_detail %in% engageL1, "Engage_L1",
                                                  ifelse(sessions$action_detail %in% engageL2, "Engage_L2",
                                                         ifelse(sessions$action_detail %in% login, "Login",
                                                                ifelse(sessions$action_detail %in% p4, "P4",
                                                                       ifelse(sessions$action_detail %in% search, "Search",
                                                                              ifelse(sessions$action_detail %in% deactivate, "Deactivate",
                                                                                     ifelse(sessions$action_detail %in% query, "Query",
                                                                                            ifelse(sessions$action_detail %in% wishlist, "Wishlist",
                                                                                                   ifelse(sessions$action_detail %in% pre_book, "Pre_Booking",
                                                                                                          ifelse(sessions$action_detail %in% book, "Booking",
                                                                                                                 ifelse(sessions$action_detail %in% post_book, "Post_Booking",
                                                                                                                        "Other")))))))))))))
# sessions$sessionFlag <- 1
# a <- sessions[!duplicated(sessions$user_id),]
# alldata <- merge(alldata, a[, c("user_id", "sessionFlag")], by.x = "id", by.y = "user_id", all.x = TRUE)
# alldata$sessionFlag[is.na(alldata$sessionFlag)] <- 0

sessions <- sessions[!is.na(sessions$user_id),]
sessions$uid <- as.integer(as.factor(sessions$user_id))
sessions <- sessions %>% group_by(uid) %>% mutate(sessionLength = n())
sessions <- sessions %>% group_by(uid) %>% mutate(totalSecsElapsed = sum(secs_elapsed, na.rm = TRUE))
sessions <- sessions %>% group_by(uid) %>% mutate(distinctActions = length(unique(action)))
sessions <- sessions %>% group_by(uid) %>% mutate(distinctActionTypes = length(unique(action_type)))

sessions$action <- str_replace_all(sessions$action, "_", "")
sessions$action[is.na(sessions$action)] <- "NotKnown"
actionSummary <- sessions %>% group_by(user_id) %>% summarise(consAction = paste(action, collapse = " "))
actionDetailSummary <- sessions %>% group_by(user_id) %>% summarize(consActionDet = paste(summarizedAction, collapse = " "))
sessions <- sessions[!duplicated(sessions$user_id),]



alldata <- merge(alldata, sessions[, c("user_id", "sessionLength", "totalSecsElapsed", "distinctActions", "distinctActionTypes")], by.x = "id", by.y = "user_id", all.x = TRUE)
alldata <- merge(alldata, actionDetailSummary, by.x = "id", by.y = "user_id", all.x = TRUE)  # merge the session and user data
alldata$consAction[is.na(alldata$consAction)] <- "noSessions"
alldata$consActionDet[is.na(alldata$consActionDet)] <- "noSessions"
alldata[is.na(alldata)] <- -1

alldata <- alldata[order(-alldata$trainFlag),] # order the data in train -> test manner for easy splitting


target <- recode(alldata[alldata$trainFlag == 1,]$country_destination,
                 "'NDF'=0; 
                 'US'=1; 
                 'other'=2; 
                 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")

testID <- alldata[alldata$trainFlag == 0,]$id


cp <- Corpus(VectorSource(alldata$consActionDet))
dtm <- DocumentTermMatrix(cp, control = list(weighting = weightTfIdf))
dtm_matrix <-   sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
                             dims=c(dtm$nrow, dtm$ncol))

trainC <- nrow(train)
testC <- nrow(train) + nrow(test)

denseFeatures <- names(alldata)[!names(alldata) %in% c("id", "date_account_created", "timestamp_first_active", "country_destination", "trainFlag",
                                                       "fac_date", "sessionFlag", "consActionDet", "consAction")]

trainD <- cBind(Matrix(as.matrix(alldata[, denseFeatures][1:trainC,]), sparse = TRUE), dtm_matrix[1:trainC,])
testD <- cBind(Matrix(as.matrix(alldata[, denseFeatures][(trainC+1) : testC,]), sparse = TRUE), dtm_matrix[(trainC+1): testC,])

save(trainD, file = "./data_V3/trainD.RData")
save(testD, file  = "./data_V3/testD.RData")
target <- target[1:trainC]
labels <- data.frame(country_destination = target)
write_csv(labels, "./data_V3/labels.csv")
ids <- data.frame(id = testID)
write_csv(ids, "./data_V3/test_user_ids.csv")