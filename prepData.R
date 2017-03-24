# import/source necesary scripts
source('../createTrainingMatchups.R') # needs to be modified to create matchups for 2017
source('../getWinPctage.R')
source('../getShootingPercentages.R')
source('../getAssistsNturnovers.R')
source('../getReboundStats.R')
source('../winsLst6.R')
source('../getTourneySeed.R')
source('../labelTeamStats.R')
source('../createSubmissionFile.R')

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

prepTrainingData <- function(season.years, createTrainMatchups = T) {
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
    
    team_metrics <- stats
    
    if (createTrainMatchups == T){
        train_df <- createTrainingMatchups(season.years)
        
        stats.w <- stats
        colnames(stats.w) <- labelTeamStats('w', stats)
        stats.l <- stats
        colnames(stats.l) <- labelTeamStats('l', stats)
        # creating final df
        team_metrics.w <- merge(train_df, stats.w, by = c('wteam', 'season'))
        team_metrics.t <- merge(team_metrics.w, stats.l, by = c('lteam', 'season'))
        
        # reordering columns
        wcols <- names(team_metrics.t[,grep('w_', names(team_metrics.t))])
        lcols <- names(team_metrics.t[,grep('l_', names(team_metrics.t))])
        team_metrics <- team_metrics.t[, c("season", "matchup", "win", "wteam", "w_TEAM_NAME",
                                           "lteam", "l_TEAM_NAME", wcols[-which(wcols == 'w_TEAM_NAME')],
                                           lcols[-which(lcols == 'l_TEAM_NAME')])]
        team_metrics$win <- as.factor(team_metrics$win)
        team_metrics$wteam <- as.factor(team_metrics$wteam)
        team_metrics$lteam <- as.factor(team_metrics$lteam)
    }
    
    team_metrics <- team_metrics[order(team_metrics$season),]
    
    # chaning classes of certain vars
    # team_metrics$season <- as.factor(team_metrics$season)
    return(team_metrics)
}

prepTestingData <- function(season.years){
    newMatchups <- submissionFile(season.years)
    teamMetrics <- prepTrainingData(season.years, F)
    teamMetrics.b <- teamMetrics
    colnames(teamMetrics) <- labelTeamStats('a', teamMetrics)
    colnames(teamMetrics.b) <- labelTeamStats('b', teamMetrics.b)
    newMatchupIDs <- t(as.data.frame(str_split(as.character(newMatchups$matchup), '_')))
    newMatchupIDs <- data.frame('ateamID' = newMatchupIDs[,2], 'bteamID' = newMatchupIDs[,3])
    row.names(newMatchupIDs) <- 1:nrow(newMatchupIDs)
    newMatchups <- cbind(newMatchups, newMatchupIDs)
    
    ateam.df <- data.frame()
    for (i in newMatchupIDs$ateamID){
        ateam.df <- rbind(ateam.df, teamMetrics[match(i, teamMetrics$TEAMID), ])
    }
    
    bteam.df <- data.frame()
    for (i in newMatchupIDs$bteamID){
        bteam.df <- rbind(bteam.df, teamMetrics.b[match(i, teamMetrics.b$TEAMID), ])
    }
    # remove teamID columns
    ateam.df <- ateam.df[, -c(1, which(names(ateam.df) == 'season'))]
    bteam.df <- bteam.df[, -c(1, which(names(ateam.df) == 'season'))]
    
    # putting it all together
    newMatchups <- cbind(newMatchups, ateam.df, bteam.df)
    
    # reordering columns
    acols <- names(newMatchups[,grep('a_', names(newMatchups))])
    bcols <- names(newMatchups[,grep('b_', names(newMatchups))])
    newMatchups <- newMatchups[, c("matchup", "win", "ateamID", "a_TEAM_NAME",
                                       "bteamID", "b_TEAM_NAME", acols[-which(acols == 'a_TEAM_NAME')],
                                       bcols[-which(bcols == 'b_TEAM_NAME')])]
    newMatchups$win <- as.factor(newMatchups$win)
    return(newMatchups)
}
