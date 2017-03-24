labelTeamStats <- function(prefix, df){
    col_names <- character()
    # setting up team stats
    for (j in colnames(df)){
        if (j == 'TEAMID' & prefix == 'w' ){
            col_names <- c(col_names, 'wteam')
        }else if (j == 'TEAMID' & prefix == 'a'){
            col_names <- c(col_names, 'TEAMID')
        }else if (j == 'TEAMID' & prefix == 'l'){
            col_names <- c(col_names, 'lteam')
        }else if (j == 'TEAMID' & prefix == 'b'){
            col_names <- c(col_names, 'TEAMID')
        }else if (j == 'season'){
            col_names <- c(col_names, j)
        }else{
            col_names <- c(col_names, paste(prefix,j, sep = '_'))
        }
    } 
    col_names
}