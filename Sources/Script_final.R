# Loading the libraries
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(plyr)

# Loading the test and train data
df_train = read_csv("../input/train_users_2.csv")
df_test = read_csv("../input/test_users.csv")

# Spliting the labels from the train data
labels = df_train['country_destination']
df_train = df_train[-grep('country_destination', colnames(df_train))]

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

# one-hot-encoding features
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

# Loading the sessions file
df_sessions = read_csv("../input/sessions.csv")

# Device
# On récupère les levels de la variable 'Device' de sessions.csv
vars <- levels(as.factor(df_sessions$device))

# Pour chaque level, on crée une variable dans df_all_combined et on
# l'initialise à -1 (pas d'informations)
for(i in 1:length(vars))
{
  df_all_combined[,vars[i]] <- -1
}
# Pour chaque personne présente dans sessions.csv, on va indiquer à
# df_all_combined que on a une information sur l'utilisateur et que à priori
# il n'a pas utilisé ce device
people <- as.factor(df_sessions$user_id)
df_all_combined[df_all_combined$id %in% people,vars] <- 0

# Boucle sur les nouvelles variables one-hot. On remplit le df_all_combined
# de 1 pour les utilisateurs qui possèdent les différents devices.
for (i in 1:length(vars))
{
  print(i)
  sousens <- as.factor(df_sessions[df_sessions$device %in% vars[i],]$user_id)
  df_all_combined[df_all_combined$id %in% sousens, vars[i]] <- 1
}

# Action
comptes <- levels(as.factor(df_all_combined$id))
df_sessions <- df_sessions[df_sessions$user_id %in% comptes,]

vars <- levels(as.factor(df_sessions$action_type))

for(i in 1:length(vars))
{
  df_all_combined[,vars[i]] <- -1
}
people <- as.factor(df_sessions$user_id)
df_all_combined[df_all_combined$id %in% people,vars] <- 0

for (i in 1:length(vars))
{
  print(i)
  sousens <- as.factor(df_sessions[df_sessions$action_type %in% vars[i],]$user_id)
  x <- tapply(sousens,sousens,length)
  z <- order(df_all_combined[df_all_combined$id %in% sousens, "id"])
  df_all_combined[df_all_combined$id %in% sousens, vars[i]] <- as.vector(x)[z]
}

# Action detail
vars <- levels(as.factor(df_sessions$action_detail))

for(i in 1:length(vars))
{
  df_all_combined[,vars[i]] <- -1
}
people <- as.factor(df_sessions$user_id)
df_all_combined[df_all_combined$id %in% people,vars] <- 0

for (i in 1:length(vars))
{
  print(i)
  sousens <- as.factor(df_sessions[df_sessions$action_detail %in% vars[i],]$user_id)
  tt <- sample(sousens, length(sousens))
  df_all_combined[df_all_combined$id %in% sousens, vars[i]] <- as.vector(tapply(tt,tt,length))
}

# Action Type
vars <- levels(as.factor(df_sessions$action_type))

for(i in 1:length(vars))
{
  df_all_combined[,vars[i]] <- -1
}
people <- as.factor(df_sessions$user_id)
df_all_combined[df_all_combined$id %in% people,vars] <- 0

for (i in 1:length(vars))
{
  print(i)
  sousens <- as.factor(df_sessions[df_sessions$action_type %in% vars[i],]$user_id)
  tt <- sample(sousens, length(sousens))
  df_all_combined[df_all_combined$id %in% sousens, vars[i]] <- as.vector(tapply(tt,tt,length))
}

# split train and test
X = df_all_combined[df_all_combined$id %in% df_train$id,]
y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$id %in% df_test$id,]

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

submission <- NULL
submission$id <- ids
submission$country <- predictions_top5

# generate submission file
submission <- as.data.frame(submission)
write.csv(submission, "submission.csv", quote=FALSE, row.names = FALSE)
