library(xgboost)
library(caret)
library(tidyverse)
library(dplyr)
library(gbm)
library(pROC)

set.seed(5627)

df18 <-  read.table("modeldata2018.csv", header=TRUE, sep=",", na.strings=" ") 
df19 <-  read.table("modeldata2019.csv", header=TRUE, sep=",", na.strings=" ") 
df20 <-  read.table("modeldata2020.csv", header=TRUE, sep=",", na.strings=" ") 

train <- rbind(df18,df19)
test <- df20

train <- train %>% 
  mutate(is_tackle = ifelse(event == 'tackle', "yes", "no"))
train$is_tackle  <- as.factor(train$is_tackle)


#train <- na.omit(train) 
#test <- na.omit(test) 

drops <-c("yardlineNumber","numMissedTackler","dir_ret","speed_ret","y_returner","x_returner","return_yardage","diff", "playResult", "kickReturnYardage", "penaltyYards", "missedTackler", "tackler", "gunners", "puntRushers", "specialTeamsSafeties", "vises", "season", "diff", "returnerId", "return_proportion", "kick_proportion", "dist_to_endzone", "carrier_area", "return_area", "kick_area", "return_yds_remaining", "path_frechet", "possessionTeam", "event")

drops2 <- c("returnerId", "return_proportion", "kick_proportion", "dist_to_endzone", "carrier_area", "return_area", "kick_area", "return_yds_remaining", "path_frechet", "possessionTeam", "event")

train_tackle = train[ , !(names(train) %in% drops)]
test_tackle = test[ , !(names(test) %in% drops)]


#train_merge = train[ , (names(train) %in% drops2)]
#test_merge = test[ , (names(test) %in% drops2)]

train_tackle = na.omit(train_tackle)
test_tackle = na.omit(test_tackle)
#train_tackle = train_tackle[1:1000,]
#train_tackle2 = train_tackle[1:5000, ]

###########train tackle model####################

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
                     

ctrl$sampling <- "up"

set.seed(5627)

mod<- train(is_tackle ~ .,
                  data = train_tackle[,-c(1,2,3)],
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

test_roc <- function(model, data) {
  
  roc(data$is_tackle,
      predict(model, data, type = "prob")[,1])

}

mod %>%
  test_roc(data = train_tackle) %>%
  auc()

preds <- predict(mod, train_tackle, type = 'prob')
preds_test <- predict(mod, test_tackle, type = 'prob')

colnames(preds) <- c("prob_no_tackle", "prob_tackle")
colnames(preds_test) <- c("prob_no_tackle", "prob_tackle")

#train = train[1:5000,]
results <- cbind(train_tackle, preds)

results_test <- cbind(test_tackle, preds_test)

write.table(results_test, file = "test.csv", sep = ",")



results2 = results[which(results$gameId == 2018090600 & results$playId == 2599),]
plot(varImp(mod), top=10)

plot(results2$prob_tackle)
lines(results2$arrival_to_carrier)
######predict yards remaining with probability of tackle and hulls####

drops <-c("event","is_tackle", "return_yardage", "kickReturnYardage", "diff", "playResult", "yardlineNumber", "missedTackler", "tackler", "specialTeamsSafeties", "vises", "puntRushers", "gunners", "numMissedTackler", "penaltyYards")

results_train = results[ , !(names(results) %in% drops)]
results_test2 = results_test[ , !(names(results_test) %in% drops)]

#results_train2 = results_train[1:1000,]
mod2 <- train(return_yds_remaining ~ .,
                  data = results_train[,-c(1,2,3)],
                  method = "gbm",
                  verbose = FALSE)
par(mfrow=c(1,2))            
p<- plot(varImp(mod2), top=10)
s<- plot(varImp(mod),top=10, col='red')


preds_yards <- predict(mod2, results_train)

results2 <- cbind(results_train, preds_yards)


preds_yards_test <- predict(mod2, results_test2)

results_final <- cbind(results_test2, preds_yards_test)


results_final$RYAE =  results_final$preds_yards_test - results_final$return_yds_remaining


write.table(results_final, file = "preds_lstm.csv", sep = ",")



