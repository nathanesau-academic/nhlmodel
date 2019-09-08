# get data from before 2007-2008 season
# advanced metrics aren't available before this time period
# may not be appropriate due to rule changes over time (i.e. change to shootout)
# and goal inflation or goal deflation (i.e. 80's oilers scored more goals than 
# teams today)
# 1996 corresponding to 1995-1996 season -> seasons before this 
# weren't 82 games
# some seasons differ with num cols due to rule changes. ex. OL, T, W, L
library(XML) 

setwd("~/Documents/Stat350Project/")

season_url <- function(year="2016") { # site is hockey-reference.com
    paste0("http://www.hockey-reference.com/leagues/",
           "NHL_", year, ".html")
}

years <- c(seq(2001,2004,1),seq(2006,2015,1))
# 1996 corresponds to 1995-1996 season
# ...
# 1998 last year with AvAge unavailable 
# 1999 AvAge available 
# 2000 last year with league average unavailable
# 2001 league average available
# ...
# 2005 no data (lockout)
# 2006 start of AvAge available
# 2013 was a lockout year (shortened season) 
# ...
# 2015 was the last completed season, 2014-2015

for(i in 1:length(years)) {
    year <- years[i]
    season_data <- readHTMLTable(season_url(year)) # convert html tables to data.frame
    season_data_names <- names(season_data)
    season_data_cols <- which(season_data_names == "teams") 
    league_season_data <- season_data[[season_data_cols[1]]][31,] # league aggregate
    season_data <- season_data[[season_data_cols[1]]][1:30,] # team stats
    league_season_data <- cbind(year,league_season_data) # keep track of year
    names(league_season_data)[1] <- "Season" # rename season column
    season_data <- cbind(rep(year,30),season_data) 
    names(season_data)[1] <- "Season"
    if(i==1) {
        cols <- ((names(season_data)!="W") * (names(season_data)!="L") * (names(season_data)!="T") * 
                   (names(season_data)!="OL"))*(1:length(season_data[1,]))
        cols <- cols[cols!=0]
        all_season_data <- season_data[,cols] # team stats (all seasons)
        all_league_season_data <- league_season_data[,cols] # league aggregate (all seasons)
    } else {
        cols <- ((names(season_data)!="W") * (names(season_data)!="L") * (names(season_data)!="T") * 
                   (names(season_data)!="OL"))*(1:length(season_data[1,]))
        cols <- cols[cols!=0]
        all_season_data <- rbind(all_season_data, season_data[,cols])
        all_league_season_data <- rbind(all_league_season_data, league_season_data[,cols])
    }
}
all_season_data <- all_season_data[,-c(25)] # Remove junk cols
all_league_season_data <- all_league_season_data[,-c(2,3,10,11,25)]
row.names(all_league_season_data) <- 1:length(all_league_season_data[,1])

# convert cols to numeric
all_league_season_data <-
    data.frame(apply(all_league_season_data, 2, function(x) as.numeric(as.character(x))))
all_season_data[,c(1,2,4:24)] <-
    data.frame(apply(all_season_data[,c(1,2,4:24)], 2, function(x) as.numeric(as.character(x))))

all_league_season_data_historical <- all_league_season_data
all_season_data_historical <- all_season_data

save(all_league_season_data_historical, file="Data/all_league_season_data_historical.Rdata")
save(all_season_data_historical, file="Data/all_season_data_historical.Rdata")
