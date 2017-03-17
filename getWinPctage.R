getWinPctage <- function(season.years) {
    require(dplyr)
    season <- vector()
    team <- vector()
    winpct <- vector()
    gmsplyd <- vector()
    for (i in season.years){
        year <- as.integer(i)
        playoff_teams <- sort(tourney_seeds$Team[which(tourney_seeds$Season == year)])

        # stats for year being considered
        target.season <- reg.season.detailed[which(reg.season.detailed$Season == year),]
        games_played.w <-target.season%>% group_by(Wteam) %>% count %>% rename(wins = n, TEAMID = Wteam)
        games_played.l <-target.season%>% group_by(Lteam) %>% count %>% rename(losses = n, TEAMID = Lteam)
        games_played <- merge(games_played.w, games_played.l, by = 'TEAMID')
        games_played$tot_games_in_season <- games_played$wins + games_played$losses
        for (j in playoff_teams){
            # find appropriate row for team id being considered
            val <- match(j, games_played$TEAMID)
            row <- games_played[val,]
            # pull values and append them to appropriate lists
            team <- c(team, j)
            season <- c(season, year)
            winpct <- c(winpct, round(row$wins / row$tot_games_in_season, digits = 3))
            gmsplyd <- c(gmsplyd, row$tot_games_in_season)
        }
    }
    df <- data.frame('TEAMID' = team, 'season' = season, 'twpct' = winpct, 'gmsplyd' = gmsplyd)
}



