getAssistsNturnovers <- function(season.years) {
    require(dplyr)
    season <- vector()
    team <- vector()
    to.pg <- vector()
    apg <- vector()
    for (i in season.years){
        year <- as.integer(i)
        playoff_teams <- sort(tourney_seeds$Team[which(tourney_seeds$Season == year)])
        
        # total games played
        target.season <- reg.season.detailed[which(reg.season.detailed$Season == year),]
        games_played.w <-target.season%>% group_by(Wteam) %>% count %>% rename(wins = n, TEAMID = Wteam)
        games_played.l <-target.season%>% group_by(Lteam) %>% count %>% rename(losses = n, TEAMID = Lteam)
        games_played <- merge(games_played.w, games_played.l, by = 'TEAMID')
        games_played$games_in_season <- games_played$wins + games_played$losses
        games_played <- games_played[,c(1,4)]
        
        # turnovers per game & assists per game
        turnovers.w <-target.season%>% group_by(Wteam) %>% rename(TEAMID = Wteam) %>%
            summarise(tot.to.w =sum(Wto), tot.asst.w = sum(Wast))
        turnovers.l <-target.season%>% group_by(Lteam) %>% rename(TEAMID = Lteam) %>%
            summarise(tot.to.l =sum(Lto), tot.asst.l = sum(Last))
        dist.stats <- merge(turnovers.l, turnovers.w, by = 'TEAMID')
        dist.stats <- merge(dist.stats, games_played, by = 'TEAMID')
        
        for (j in playoff_teams){
            # find appropriate row for team id being considered
            val <- match(j, dist.stats$TEAMID)
            row <- dist.stats[val,]
            # pull values and append them to appropriate lists
            team <- c(team, j)
            season <- c(season, year)
            to.pg <- c(to.pg, round((row$tot.to.l + row$tot.to.w) / row$games_in_season, digits = 2))
            apg <- c(apg, round((row$tot.asst.l + row$tot.asst.w) / row$games_in_season, digits = 2))
        }
    }
    df <- data.frame('TEAMID' = team, 'season' = season, 'turnovers.pg' = to.pg, 'assists.pg' = apg)
}
