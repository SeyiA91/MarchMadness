getShootingPercentages <- function(season.years) {
    require(dplyr)
    season <- vector()
    team <- vector()
    fgp <- vector()
    ftp <- vector()
    tpfgp <- vector()
    tpapg <- vector()
    fgapg <- vector()
    ppg <- vector()
    for (i in season.years){
        year <- as.integer(i)
        playoff_teams <- sort(tourney_seeds$Team[which(tourney_seeds$Season == year)])
        
        # stats for year being considered
        target.season <- reg.season.detailed[which(reg.season.detailed$Season == year),]
        target.season$Wteam <- as.factor(target.season$Wteam)
        target.season$Lteam <- as.factor(target.season$Lteam)
        target.season <- reg.season.detailed[which(reg.season.detailed$Season == year),]
        # getting game stats to calculate ppg, 3pt attempts/ gm, and fg attempts/game
        games_played.w <-target.season%>% group_by(Wteam) %>% count %>% rename(wins = n, TEAMID = Wteam)
        games_played.l <-target.season%>% group_by(Lteam) %>% count %>% rename(losses = n, TEAMID = Lteam)
        games_played <- merge(games_played.w, games_played.l, by = 'TEAMID')
        games_played$tot.games.in.season <- games_played$wins + games_played$losses
        # gathering shooting stats for teams when they appear in either wteam or lteam
        shots_table.w <- target.season %>% group_by(Wteam) %>% rename(TEAMID = Wteam) %>%
            summarize(total.Wfga = sum(Wfga), total.Wfgm = sum(Wfgm), total.W3fga = sum(Wfga3),total.W3fgm = sum(Wfgm3),
                      total.Wfta = sum(Wfta), total.Wftm = sum(Wftm))
        shots_table.l <- target.season %>% group_by(Lteam) %>% rename(TEAMID = Lteam) %>% 
            summarize(total.Lfga = sum(Lfga), total.Lfgm = sum(Lfgm), total.L3fga = sum(Lfga3),total.L3fgm = sum(Lfgm3),
                      total.Lfta = sum(Lfta), total.Lftm = sum(Lftm))
        shots_table <- merge(shots_table.w, shots_table.l, by = 'TEAMID')
        shots_table <- merge(shots_table, games_played, by = 'TEAMID')
        # creating shot totals
        shots_table$total.fga <- shots_table$total.Wfga + shots_table$total.Lfga
        shots_table$total.fgm <- shots_table$total.Wfgm + shots_table$total.Lfgm
        shots_table$total.3fga <- shots_table$total.W3fga + shots_table$total.L3fga
        shots_table$total.3fgm <- shots_table$total.W3fgm + shots_table$total.L3fgm
        shots_table$total.fta <- shots_table$total.Wfta + shots_table$total.Lfta
        shots_table$total.ftm <- shots_table$total.Wftm + shots_table$total.Lftm
        myVars <- c("TEAMID","total.fga","total.fgm","total.3fga","total.3fgm","total.fta","total.ftm","tot.games.in.season")
        shots_table <- shots_table[myVars]
        
        for (j in playoff_teams){
            # find appropriate row for team id being considered
            val <- match(j, shots_table$TEAMID)
            row <- shots_table[val,]
            # pull values and append them to appropriate lists
            team <- c(team, j)
            season <- c(season, year)
            fgp <- c(fgp, round(row$total.fgm / row$total.fga, digits = 3))
            tpfgp <- c(tpfgp, round(row$total.3fgm / row$total.3fga, digits = 3))
            ftp <- c(ftp, round(row$total.ftm / row$total.fta, digits = 3))
            tpapg <- c(tpapg, round(row$total.3fga / row$tot.games.in.season, digits = 0))
            fgapg <- c(fgapg, round(row$total.fga / row$tot.games.in.season, digits = 0))
            ppg <- c(ppg, round(sum((row$total.fga / row$tot.games.in.season)*(row$total.fgm / row$total.fga)*2,
                                    (row$total.3fga / row$tot.games.in.season)*(row$total.3fgm / row$total.3fga)*3),digits = 2))
        }
    }
    df <- data.frame('TEAMID' = team, 'season' = season, 'fgp' = fgp, 'three.pt.fgp' = tpfgp, 'freethrow.p' = ftp, 
                     'three.pt.attempts.pg' = tpapg, 'field.goal.attempts.pg' = fgapg, 'ppg' = ppg)
}