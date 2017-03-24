createTrainingMatchups <- function(season.years){
    team <- vector()
    result <- vector()
    wteam <- vector()
    wteam_name <- vector()
    lteam <- vector()
    lteam_name <- vector()
    season <- vector()
    for (i in season.years){
        year <- as.integer(i)
        season_matches <- tourney_results.c[which(tourney_results.c$Season == year),]
        names(season_matches) <- tolower(names(season_matches))
        teams <- teams; colnames(teams)[1] <- 'Team' 
        for (j in c(1:nrow(season_matches))) {
            season <- c(season, year)
            row <- season_matches[j,]
            # find appropriate row for team id being considered in team df
            if (row$wteam < row$lteam) {
                vector <- paste(year,row$wteam,row$lteam, sep = '_')
                team <- c(team, vector)
                result <- c(result, 1)
                wteam <- c(wteam, row$wteam)
                lteam <- c(lteam, row$lteam)
                wteam_name <- c(wteam_name, teams[match(row$wteam, teams$Team),]$Team_Name) 
                lteam_name <- c(lteam_name, teams[match(row$lteam, teams$Team),]$Team_Name)
            } else {
                oth <- paste(year, row$lteam, row$wteam, sep = '_')
                team <- c(team, oth)
                result <- c(result, 0)
                wteam <- c(wteam, row$wteam)
                lteam <- c(lteam, row$lteam)
                wteam_name <- c(wteam_name, teams[match(row$wteam, teams$Team),]$Team_Name) 
                lteam_name <- c(lteam_name, teams[match(row$lteam, teams$Team),]$Team_Name)
            }
        }
    }
    train_df <- data.frame('matchup' = team, 'season' = season, 'win' = result, 'wteam' = wteam,
                            'lteam' = lteam)
    # train_df <- cbind(train_df, season_matches$wteam, season_matches$lteam) , 'lteam_name' = lteam_name
    # colnames(train_df) <- c('matchup', 'win', 'wteam', 'lteam') 'wteam_name' = wteam_name,
}