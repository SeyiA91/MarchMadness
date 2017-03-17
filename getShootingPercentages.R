getShootingPercentages <- function(season.years) {
    require(dplyr)
    season <- vector()
    team <- vector()
    fgp <- vector()
    ftp <- vector()
    tpfgp <- vector()
    for (i in season.years){
        year <- as.integer(i)
        playoff_teams <- sort(tourney_seeds$Team[which(tourney_seeds$Season == year)])
        
        # stats for year being considered
        target.season <- reg.season.detailed[which(reg.season.detailed$Season == year),]
        target.season$Wteam <- as.factor(target.season$Wteam)
        target.season$Lteam <- as.factor(target.season$Lteam)
        # gathering shooting stats for teams when they appear in either wteam or lteam
        shots_table.w <- target.season %>% group_by(Wteam) %>% rename(TEAMID = Wteam) %>%
            summarize(total.Wfga = sum(Wfga), total.Wfgm = sum(Wfgm), total.W3fga = sum(Wfga3),total.W3fgm = sum(Wfgm3),
                      total.Wfta = sum(Wfta), total.Wftm = sum(Wftm))
        shots_table.l <- target.season %>% group_by(Lteam) %>% rename(TEAMID = Lteam) %>% 
            summarize(total.Lfga = sum(Lfga), total.Lfgm = sum(Lfgm), total.L3fga = sum(Lfga3),total.L3fgm = sum(Lfgm3),
                      total.Lfta = sum(Lfta), total.Lftm = sum(Lftm))
        shots_table <- merge(shots_table.w, shots_table.l, by = 'TEAMID')
        # creating shot totals
        shots_table$total.fga <- shots_table$total.Wfga + shots_table$total.Lfga
        shots_table$total.fgm <- shots_table$total.Wfgm + shots_table$total.Lfgm
        shots_table$total.3fga <- shots_table$total.W3fga + shots_table$total.L3fga
        shots_table$total.3fgm <- shots_table$total.W3fgm + shots_table$total.L3fgm
        shots_table$total.fta <- shots_table$total.Wfta + shots_table$total.Lfta
        shots_table$total.ftm <- shots_table$total.Wftm + shots_table$total.Lftm
        myVars <- c("TEAMID","total.fga","total.fgm","total.3fga","total.3fgm","total.fta","total.ftm")
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
        }
    }
    df <- data.frame('TEAMID' = team, 'season' = season, 'fgp' = fgp, 'tpfgp' = tpfgp, 'ftp' = ftp)
}