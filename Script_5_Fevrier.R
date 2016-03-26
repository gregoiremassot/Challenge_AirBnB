# load libraries
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(data)
# library(plyr)
# library(dplyr)


# load data

df_train = read_csv("../input/train_users_2.csv")
df_test = read_csv("../input/test_users.csv")
labels = df_train['country_destination']
df_train = df_train[-grep('country_destination', colnames(df_train))]
df_sessions = read_csv("../input/sessions.csv")

# combine train and test data
df_all = rbind(df_train,df_test)
# remove date_first_booking
df_all = df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]
# replace missing values
df_all[is.na(df_all)] <- -1

# split date_account_created in year, month and day
dac = as.data.frame(str_split_fixed(df_all$date_account_created, '-', 3))
df_all['dac_year'] = dac[,1]
df_all['dac_month'] = dac[,2]
df_all['dac_day'] = dac[,3]
df_all = df_all[,-c(which(colnames(df_all) %in% c('date_account_created')))]

# split timestamp_first_active in year, month and day
df_all[,'tfa_year'] = substring(as.character(df_all[,'timestamp_first_active']), 1, 4)
df_all['tfa_month'] = substring(as.character(df_all['timestamp_first_active']), 5, 6)
df_all['tfa_day'] = substring(as.character(df_all['timestamp_first_active']), 7, 8)
df_all = df_all[,-c(which(colnames(df_all) %in% c('timestamp_first_active')))]

# clean Age by removing values
# df_all[df_all$age < 14 | df_all$age > 100,'age'] <- -1

# one-hot-encoding features
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)


# Device
var2 <- levels(as.factor(df_sessions$device))

for(i in 1:length(var2))
{
  df_all_combined[,var2[i]] <- -1
}
people <- as.factor(df_sessions$user_id)
df_all_combined[df_all_combined$id %in% people,var2] <- 0
sousens <- df_sessions[df_sessions$device %in% var2,]

for (i in 1:length(var2))
{
  print(i)
  sousens2 <- as.factor(df_sessions[df_sessions$device %in% var2[i],]$user_id)
  df_all_combined[df_all_combined$id %in% sousens2, var2[i]] <- 1
}

# Action
comptes <- levels(as.factor(df_all_combined$id))
df_sessions <- df_sessions[df_sessions$user_id %in% comptes,]

var2 <- levels(as.factor(df_sessions$action_type))

for(i in 1:length(var2))
{
  df_all_combined[,var2[i]] <- -1
}
people <- as.factor(df_sessions$user_id)
df_all_combined[df_all_combined$id %in% people,var2] <- 0
sousens <- df_sessions[df_sessions$action_type %in% var2,]

for (i in 1:length(var2))
{
  print(i)
  sousens2 <- as.factor(df_sessions[df_sessions$action_type %in% var2[i],]$user_id)
  x <- tapply(sousens2,sousens2,length)
  z <- order(df_all_combined[df_all_combined$id %in% sousens2, "id"])
  df_all_combined[df_all_combined$id %in% sousens2, var2[i]] <- as.vector(x)[z]
}

# Action detail
var2 <- levels(as.factor(df_sessions$action_detail))

for(i in 1:length(var2))
{
  df_all_combined[,var2[i]] <- -1
}
people <- as.factor(df_sessions$user_id)
df_all_combined[df_all_combined$id %in% people,var2] <- 0
sousens <- df_sessions[df_sessions$action_detail %in% var2,]

for (i in 1:length(var2))
{
  print(i)
  sousens2 <- as.factor(df_sessions[df_sessions$action_detail %in% var2[i],]$user_id)
  tt <- sample(sousens2, length(sousens2))
  df_all_combined[df_all_combined$id %in% sousens2, var2[i]] <- as.vector(tapply(tt,tt,length))
  # print(as.vector(tapply(tt,tt,length)))
}

# Action Type
var2 <- levels(as.factor(df_sessions$action_type))

for(i in 1:length(var2))
{
  df_all_combined[,var2[i]] <- -1
}
people <- as.factor(df_sessions$user_id)
df_all_combined[df_all_combined$id %in% people,var2] <- 0
sousens <- df_sessions[df_sessions$action_type %in% var2,]

for (i in 1:length(var2))
{
  print(i)
  sousens2 <- as.factor(df_sessions[df_sessions$action_type %in% var2[i],]$user_id)
  tt <- sample(sousens2, length(sousens2))
  df_all_combined[df_all_combined$id %in% sousens2, var2[i]] <- as.vector(tapply(tt,tt,length))
  # print(as.vector(tapply(tt,tt,length)))
}

# Durée des actions

var2 <- levels(as.factor(df_sessions$action))

for(i in 1:length(var2))
{
  df_all_combined[,paste("meantime", var2[i], sep= "")] <- -1
}
people <- as.factor(df_sessions$user_id)
df_all_combined[df_all_combined$id %in% people,var2] <- 0
sousens <- df_sessions[df_sessions$action %in% var2,]

for (i in 1:length(var2))
{
  print(i)
  # i <- 15
  sousens2 <- df_sessions[df_sessions$action %in% var2[i],c("user_id","secs_elapsed")]
  nomvar <- paste("meantime", var2[i], sep= "")
  u <- as.factor(df_all_combined[df_all_combined$id %in% as.factor(sousens2$user_id),]$id)
  df_all_combined[df_all_combined$id %in% as.factor(sousens2$user_id), nomvar] <- 
    ddply(sousens2[sousens2$user_id %in% u,], .(user_id), summarize, sum=sum(secs_elapsed))$sum
  
  df_all_combined[df_all_combined$id %in% as.factor(sousens2$user_id), nomvar][is.na(df_all_combined[df_all_combined$id %in% as.factor(sousens2$user_id), nomvar])] <- -1
  df_all_combined[df_all_combined$id %in% as.factor(sousens2$user_id), nomvar]
  # print(as.vector(tapply(tt,tt,length)))
}

write.table(df_all_combined, file="df_all_combined.csv", sep=",", row.names = FALSE)

# split train and test
X = df_all_combined[df_all_combined$id %in% df_train$id,]
y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$id %in% df_test$id,]

# tuneGrid <- expand.grid(max_depth = c(1,2,3,4,5,6,7,8,9,10),
#                         nrounds = 10,
#                         eta = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

# tuneGrid <- expand.grid(nrounds = c(125,150,160,170,175,180,190,200),
#                         max_depth = c(2,3,4,5,6,7),
#                         eta = c(0.05,0.06,0.07,0.08,0.09,0.1),
#                         gamma = c(1),
#                         colsample_bytree = c(0.2,0.4,0.6,0.8),
#                         min_child_weight = c(1)
#                         )
# library(doSNOW)
# registerDoSNOW(8)
# tuneGrid <- expand.grid(max_depth = c(3),
#                         nrounds = c(100,125,150,175,200),
#                         # nrounds = c(100,125,150,175,200),
#                         eta = c(0.06,0.07,0.08,0.1),
#                         gamma = c(0),
#                         # colsample_bytree = c(0.2,0.4,0.5,0.6,0.8),
#                         colsample_bytree = c(0.4),
#                         min_child_weight = c(1))
# 
# fitControl <- trainControl(method = "repeatedcv",
#                            number = 3,
#                            repeats = 1,
#                            verboseIter = TRUE,
#                            allowParallel = TRUE)
# xvalXGB <- train(y2 ~ .,
#                  data = data.matrix(X[,-1]),
#                  method = "xgbTree",
#                  tuneGrid = tuneGrid,
#                  trControl = fitControl
#                  )

# train xgboost

# df_all_combined <- read_csv("df_all_combined.csv", col_names = TRUE)

xgb <- xgboost(data = data.matrix(X[,-1]), 
               label = y, 
               eta = 0.4,
               max_depth = 3, 
               nround=75, 
               subsample = 0.6,
               colsample_bytree = 0.75,
               seed = 0,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 8
               # missing = NaN
)

# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test[,-1]))

# extract the 5 classes with highest probabilities
predictions <- as.data.frame(matrix(y_pred, nrow=12))
rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))

# create submission 
idx = X_test$id
id_mtx <-  matrix(idx, 1)[rep(1,5), ]
ids <- c(id_mtx)
# 10,000 times faster in my local testing:
# system.time({idx = X_test$id; id_mtx <-  matrix(idx, 1)[rep(1,5), ]; ids <- c(id_mtx)})
#   user  system elapsed 
#  0.009   0.002   0.011 
# system.time({ids<-NULL; for (i in 1:NROW(X_test)){idx <- X_test$id[i]; ids <- append(ids, rep(idx,5))}})
#   user  system elapsed 
#108.056  54.932 166.038
#ids <- NULL
#for (i in 1:NROW(X_test)) {
#  idx <- X_test$id[i]
#  ids <- append(ids, rep(idx,5))
#}

submission <- NULL
submission$id <- ids
submission$country <- predictions_top5

# generate submission file
submission <- as.data.frame(submission)
write.csv(submission, "submission.csv", quote=FALSE, row.names = FALSE)

#xgb importance
# 
# names <- dimnames(X[,-1])[[2]]
# importance_matrix <- xgb.importance(names, model = xgb)
# importance_matrix$Feature[1:15]
# xgb.plot.importance(importance_matrix[1:15,])