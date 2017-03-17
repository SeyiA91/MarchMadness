setwd('~/dev/marchMayhem/March_Madness_2017/')
rm(list = ls())

# scripts needed for data transformations
source('../createTrainingMatchups.R') # needs to be modified to create matchups for 2017
source('../getWinPctage.R')
source('../getShootingPercentages.R')
source('../getAssistsNturnovers.R')
source('../getReboundStats.R')
source('../winsLst6.R')
source('../getTourneySeed.R')
source('../labelTeamStats.R')

# original datasets
require(data.table)
reg.season.c <- fread('RegularSeasonCompactResults.csv')
reg.season.detailed <-fread('RegularSeasonDetailedResults.csv')
seasons <- fread('Seasons.csv')
teams <- fread('Teams.csv')
tourney_results.c <- fread('TourneyCompactResults.csv')
tourney_results.detailed <- fread('TourneyDetailedResults.csv')
tourney_seeds <- fread('TourneySeeds.csv')
tourney_slots <- fread('TourneySlots.csv')
sample <- fread('sample_submission.csv')

# creating tournament matchup results (dependent var)
# season.years <- levels(as.factor(tourney_results.c$Season))[19:32]
season.years <- c('2016')
train_df <- createTrainingMatchups(season.years)

# Creating training data 
require(stringr)

# getting win pctage
winPctage <- getWinPctage(season.years)

# field goal percentages
shootingpct <- getShootingPercentages(season.years)

# assists and turnovers
assistsNturnovers <- getAssistsNturnovers(season.years)

# rebounds per game (offensive & defensive)
reboundStats <- getReboundStats(season.years)

# Num of wins in last 6
wins_last_six_games_by_team <- winsLst6(season.years)

# Seed in tourney
seeding <- getTourneySeed(season.years)

# Combining columns together
stats <- Reduce(function(x, y) merge(x, y, by = c('TEAMID', 'season')), list(winPctage, shootingpct, assistsNturnovers,
                                                           reboundStats, wins_last_six_games_by_team, seeding))

col_names.w <- labelTeamStats('w')
col_names.l <- labelTeamStats('l')

stats.w <- stats
colnames(stats.w) <- col_names.w
stats.l <- stats
colnames(stats.l) <- col_names.l
# creating final df
team_metrics.w <- merge(train_df, stats.w, by = c('wteam', 'season'))
team_metrics.t <- merge(team_metrics.w, stats.l, by = c('lteam', 'season'))

# reordering columns
wcols <- names(team_metrics.t[,grep('w_', names(team_metrics.t))])
lcols <- names(team_metrics.t[,grep('l_', names(team_metrics.t))])
team_metrics <- team_metrics.t[, c("season", "matchup", "win", "wteam", "lteam", wcols, lcols)]
team_metrics <- team_metrics[order(team_metrics$season),]

# chaning classes of certain vars
# team_metrics$season <- as.factor(team_metrics$season)
team_metrics$win <- as.factor(team_metrics$win)
team_metrics$wteam <- as.factor(team_metrics$wteam)
team_metrics$lteam <- as.factor(team_metrics$lteam)

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
