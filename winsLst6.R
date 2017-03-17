winsLst6 <- function(season.years){
    season <- vector()
    team <- vector()
    gmswon <- vector()
    for (i in season.years) {
        year <- as.integer(i)
        target.season <- reg.season.detailed[which(reg.season.detailed$Season == year),]
        playoff_teams <- sort(tourney_seeds$Team[which(tourney_seeds$Season == year)])
        
        for (j in playoff_teams) {
            games <- target.season[which(target.season$Wteam == j| target.season$Lteam == j), ]
            season <- c(season, i)
            team <- c(team, j)
            # tail bc we are only considering the last six games
            numwins <- sum(tail(games$Wteam) == j)
            gmswon <- c(gmswon, numwins)
        }
    }
    df <- data.frame('TEAMID' = team, 'season' = season, 'victoriesLst6' = gmswon)
}