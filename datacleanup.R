setwd('~/dev/marchMayhem/March_Madness_2017/')
rm(list = ls())

# scripts needed for data transformations
source('../prepTrainingData.R')

# season.years <- levels(as.factor(tourney_results.c$Season))[19:32]
season.years <- c('2016')

train_df <- prepTrainingData(season.years)

str(train_df)
require(lattice)
with(train_df, xyplot(win,w_twpct))

######## building model #########  
train <- subset(team_metrics, season <= 2014)
train_df <- train[,c(-c(1:2,4:5))]
train_df <- train_df[complete.cases(train_df),]
test <- subset(team_metrics, season > 2014)
test_df <- test[,c(-c(1:3,4:5))]

# library(glmnet)
# library(ROCR)
# library(caret)
# 
# options(scipen=999)
# 
# # model <- glm(win ~ ., family = 'binomial', data = train_df)
# glm_model <- caret::train(win~., data = train_df, method = 'glm', family = 'binomial')
# summary(model)

library(randomForest)  
library(e1071)  
library(caret)  
rf = randomForest(win ~., data = train_df, ntree = 100)
pred <- predict(rf, test_df)
print(confusionMatrix(data = pred,
                      reference = test$win,
                      positive = "1"))

fitControl <- trainControl(## 10-fold Crossvalidation
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 5,
    verboseIter=FALSE ,
    # PCA Preprocessing
    preProcOptions="pca",
    # With parallel backend
    allowParallel=TRUE)

rf <- caret::train(win~., data = train_df, method = 'rf', trControl = fitControl)
glm <- caret::train(win~., data = train_df, method = 'glm', family = 'binomial', trControl = fitControl)
Model <- c('Random Forest', 'GLM')
Accuracy <- c(round(max(rf$results$Accuracy),4)*100, round(max(glm$results$Accuracy),4)*100)
performance <- cbind(Model,Accuracy); performance

pred_rf <- predict(rf, test_df)
cm_rf <- table(pred_rf, test$win[complete.cases(test$win)])
length(pred_rf)

test_2016 <- subset(team_metrics, season == 2016)
test_df_2016 <- test_2016[,c(-c(1:3,4:5))]

pred_2016.rf <- predict(rf, test_2016, type = 'prob')
pred_2016.glm <- predict(glm, test_2016, type = 'prob')
glm.output <- data.frame('matchup' = test_2016$matchup, 'prediction' = pred_2016.glm)
glm.output$prediction <- ifelse(glm.output$prediction.0 > glm.output$prediction.1, 0, 1)
write.csv(glm.output, file = 'marchMadnessBracketResults.csv', row.names = F)

output <- as.data.frame(cbind(test_2016$matchup, pred_2016))









# require('rvest')
# conf.url <- 'http://www.ncaa.com/standings/basketball-men/d1'
# webpage <- read_html(conf.url)
# conf_table <- html_nodes(webpage, 'table')
# conf <- html_table(conf_table, fill = T)
# names(conf)
# conferences = read_html('http://www.ncaa.com/standings/basketball-men/d1')
# # this returns full list of conferences with teams seperated by conf names
# teamsNConferences = html_nodes(conferences, '.ncaa-standings-conference-name, .ncaa-standing-conference-team')
# length(teamsNConferences)
# teamsNConferences[1:3]
# html_text(teamsNConferences)
# # this returns just teams
# hmm <- html_nodes(conferences, 'span.ncaa-standings-conference-name, .ncaa-standing-conference-team')
# teamsByConf <- html_text(hmm)
# class(teamsByConf)
# team.list <- 
# 
# conferences <- html_nodes(conferences, '.ncass-standings-conference-name')
# conf <- html_text(conferences)
# 
# 
# intersect(names(reg.season), names(tourney_slots))
# reg_season <- merge(reg.season, seasons, by = 'season')
# reg_season_w_teams <- merge(reg_season, teams, by.x = '')
# intersect(names())
# 
# str(reg.season)
# str(seasons)
# str(tourney_results)
