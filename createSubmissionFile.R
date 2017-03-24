## Creates the submission file, every possible first round combination for each
## season
submissionFile <- function(season.years) {
    season <- as.integer(season.years)
    playoffTeams <- sort(tourney_seeds$Team[which(tourney_seeds$Season == season)])
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
    form <- data.frame("matchup" = idcol, "win" = NA)
    return(form)
}
