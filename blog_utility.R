## Functions to expedite data analysis process

## Imports a package which allows for easier regular expressions
library('stringr')

## Function which calculates team metrics by season
### Predictors Included:
### Wins
### Losses
### Total Win %
### Wins in last six games
### Seed
team_metrics_by_season <- function(seasonletter) {
    playoff_teams <- sort(tourneySeeds$team[which(tourneySeeds$season == seasonletter)])
    playoff_seeds <- tourneySeeds[which(tourneySeeds$season == seasonletter), ]
    season <- regSeason[which(regSeason$season == seasonletter), ]
    ##Each of these dataframes is labled "Var1" and "Freq" for TeamID and Statistic respectively
    #Wins (NOT A USABLEVAR, must scale)
    win_freq_table <- as.data.frame(table(season$wteam))
    wins_by_team <- win_freq_table[win_freq_table$Var1 %in% playoff_teams, ]
    #Losses (NOT A USABLEVAR, must scale)
    loss_freq_table <- as.data.frame(table(season$lteam), stringsAsFactors = FALSE)
    loss_by_team <- loss_freq_table[loss_freq_table$Var1 %in% playoff_teams, ]
    #Total Win Percentage
    gamesplayed <- as.vector(wins_by_team$Freq + loss_by_team$Freq)
    total_winpct <- round(wins_by_team$Freq / gamesplayed, digits = 3)
    total_winpct_by_team <- as.data.frame(cbind(as.vector(loss_by_team$Var1), total_winpct))
    colnames(total_winpct_by_team) <- c("Var1", "Freq")
    #Num of wins in last 6 games
    wins_last_six_games_by_team <- data.frame()
    for(i in playoff_teams) {
        games <- season[which(season$wteam == i | season$lteam == i), ]
        numwins <- sum(tail(games$wteam) == i)
        put <- c(i, numwins)
        wins_last_six_games_by_team <- rbind(wins_last_six_games_by_team, put)
    }
    colnames(wins_last_six_games_by_team) <- c("Var1", "Freq")
    #Seed
    pattern <- "[A-Z]([0-9][0-9])"
    team_seeds <- as.data.frame(str_match(playoff_seeds$seed, pattern))
    seeds <- as.numeric(team_seeds$V2)
    playoff_seeds$seed  <- seeds
    seed_col <- vector()
    for(i in playoff_teams) {
        val <- match(i, playoff_seeds$team)
        seed_col <- c(seed_col, playoff_seeds$seed[val])
    }
    team_seed <- data.frame("Var1" = playoff_teams, "Freq" =seed_col)
    team_metrics <- data.frame()
    team_metrics <- cbind(total_winpct_by_team, wins_last_six_games_by_team$Freq,
    team_seed$Freq)
    
    colnames(team_metrics) <- c("TEAMID", "A_TWPCT", "A_WST6", "A_SEED")
    return(team_metrics)
}


## Function which creates the Train data set for each season, to create the full
## Train data set loop this function through seasons A-M
train_frame_model <- function(seasonletter) {
    teamMetrics <- team_metrics_by_season(seasonletter)
    season_matches <- tourneyRes[which(tourneyRes$season == seasonletter), ]
    team <- vector()
    result <- vector()
    for(i in c(1:nrow(season_matches))) {
        row <- season_matches[i, ]
        if(row$wteam < row$lteam) {
            vector <- paste(seasonletter,"_",row$wteam,"_", row$lteam, sep ="")
            team <- c(team, vector)
            result <- c(result, 1)
        } else {
            oth <- paste(seasonletter, "_", row$lteam, "_", row$wteam, sep ="")
            team <- c(team, oth)
            result <- c(result, 0)
        }
    }
    model_data_frame <- data.frame("Matchup" = team, "Win" = result)
    teamMetrics_away <- teamMetrics
    colnames(teamMetrics_away) <- c("TEAMID", "B_TWPCT","B_WST6", "B_SEED")
    pattern <- "[A-Z]_([0-9]{3})_([0-9]{3})"
    teamIDs <- as.data.frame(str_match(model_data_frame$Matchup, pattern))
    teamIDs <- teamIDs[ , c(2,3)]
    colnames(teamIDs) <- c("HomeID", "AwayID")
    model_data_frame <- cbind(model_data_frame, teamIDs)
    home_frame <- data.frame()
    for(i in model_data_frame$HomeID) {
        home_frame <- rbind(home_frame, teamMetrics[match(i, teamMetrics$TEAMID), ])
    }
    #Removing teamID column
    home_frame <- home_frame[ , -1]
    
    away_frame <- data.frame()
    for(i in model_data_frame$AwayID) {
        away_frame <- rbind(away_frame, teamMetrics_away[match(i, teamMetrics_away$TEAMID), ])
    }
    away_frame <- away_frame[ , -1]
    
    model_data_frame <- cbind(model_data_frame, home_frame, away_frame)
    
    return(model_data_frame)
}


## Creates the "Test" data set per season
test_frame_model <- function(season) {
    model_data_frame <- submissionFile(season)
    teamMetrics <- team_metrics_by_season(season)
    teamMetrics_away <- teamMetrics
    colnames(teamMetrics_away) <- c("TEAMID", "B_TWPCT","B_WST6", "B_SEED")
    pattern <- "[A-Z]_([0-9]{3})_([0-9]{3})"
    teamIDs <- as.data.frame(str_match(model_data_frame$Matchup, pattern))
    teamIDs <- teamIDs[ , c(2,3)]
    colnames(teamIDs) <- c("HomeID", "AwayID")
    model_data_frame <- cbind(model_data_frame, teamIDs)
    home_frame <- data.frame()
    for(i in model_data_frame$HomeID) {
        home_frame <- rbind(home_frame, teamMetrics[match(i, teamMetrics$TEAMID), ])
    }
    #Removing teamID column
    home_frame <- home_frame[ , -1]
    
    away_frame <- data.frame()
    for(i in model_data_frame$AwayID) {
        away_frame <- rbind(away_frame, teamMetrics_away[match(i, teamMetrics_away$TEAMID), ])
    }
    away_frame <- away_frame[ , -1]
    
    model_data_frame <- cbind(model_data_frame, home_frame, away_frame)
    
    return(model_data_frame)
}


## Creates the submission file, every possible first round combination for each
## season
submissionFile <- function(season) {
    playoffTeams <- sort(tourneySeeds$team[which(tourneySeeds$season == season)])
    numTeams <- length(playoffTeams)
    matrix <- matrix(nrow =numTeams, ncol = numTeams)
    for(i in c(1:numTeams)) {
        for(j in c(1:numTeams)) {
            # creates all potential matchups in matrix form
            matrix[i,j] <- paste(season,"_",playoffTeams[i],"_", playoffTeams[j], sep ="")
        }
    }
    # keeps only the upper triangle so as to avoid duplications
    keep <- upper.tri(matrix, diag = F)
    idcol <- vector()
    for(i in c(1:numTeams)) {
        for(j in c(1:numTeams)) {
            if(keep[i,j] == T) {
                # creates a vecotr of all potential matchups resulting from upper tri transformation
                idcol <- c(idcol, matrix[i,j])
            }
        }
    }
    form <- data.frame("Matchup" = idcol, "Win" = NA)
    return(form)
}
