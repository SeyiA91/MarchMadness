getTourneySeed <- function(season.years) {
    team <- vector()
    season <- vector()
    seed <- vector()
    team_name <- vector()
    pattern <- "[A-Z]([0-9][0-9])"
    
    for (i in season.years){
        year <- as.integer(i)
        target.season <- reg.season.detailed[which(reg.season.detailed$Season == year),]
        playoff_seeds <- tourney_seeds[which(tourney_seeds$Season == year),]
        playoff_seeds$numeric.seed <- str_match(playoff_seeds$Seed, pattern)[,2]
        playoff_teams <- sort(tourney_seeds$Team[which(tourney_seeds$Season == year)])
        teams <- teams; colnames(teams)[1] <- 'Team' 
        teams <- teams[which(teams$Team %in% playoff_teams)]
        playoff_seeds <- merge(playoff_seeds, teams, by = 'Team')
        
        for (j in playoff_teams){
            # find appropriate row for team id being considered
            team <- c(team, j)
            season <- c(season, year)
            val <- match(j, playoff_seeds$Team)
            seed <- c(seed, playoff_seeds$numeric.seed[val])
            team_name <- c(team_name, playoff_seeds$Team_Name[val]) # not working correctly
        }
    }
    df <- data.frame('TEAMID' = team, 'TEAM_NAME' = team_name, 'season' = season, 'seed' = seed)
}