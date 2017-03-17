getReboundStats <- function(season.years) {
    require(dplyr)
    season <- vector()
    team <- vector()
    orpg <- vector()
    drpg <- vector()
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
        
        # rebounds per game (offensive & defensive)
        rebounds.w <-target.season%>% group_by(Wteam) %>% rename(TEAMID = Wteam) %>%
            summarise(tot.ofr.w = sum(Wor), tot.dfr.w = sum(Wdr))
        rebounds.l <-target.season%>% group_by(Lteam) %>% rename(TEAMID = Lteam) %>%
            summarise(tot.ofr.l = sum(Lor), tot.dfr.l = sum(Ldr))
        rebounds <- merge(rebounds.l, rebounds.w, by = 'TEAMID')
        rebounds <- merge(rebounds, games_played, by = 'TEAMID')
        
        for (j in playoff_teams){
            # find appropriate row for team id being considered
            val <- match(j, rebounds$TEAMID)
            row <- rebounds[val,]
            # pull values and append them to appropriate lists
            team <- c(team, j)
            season <- c(season, year)
            orpg <- c(orpg, round((row$tot.ofr.l + row$tot.ofr.w) / row$games_in_season, digits = 2))
            drpg <- c(drpg, round((row$tot.dfr.l + row$tot.dfr.w) / row$games_in_season, digits = 2))
        }
    }
    df <- data.frame('TEAMID' = team, 'season' = season, 'offRebs.pg' = orpg, 'defRebs.pg' = drpg)
}



# for (i in 1:nrow(rebounds)){
#     rebounds$ofrpg[i] <- round(sum(rebounds$tot.ofr.l[i],rebounds$tot.ofr.w[i])/rebounds$total.games[i], digits = 2)
#     rebounds$dfrpg[i] <- round(sum(rebounds$tot.dfr.l[i],rebounds$tot.dfr.w[i])/rebounds$total.games[i], digits = 2)
# }
