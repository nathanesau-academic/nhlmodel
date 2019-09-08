# get NHL team stats data from hockey-reference.com
library(XML)

setwd("C:/Users/Nathan/Dropbox/Stat350Project/")

season_url <- function(year="2016") { # site is hockey-reference.com
    paste0("http://www.hockey-reference.com/leagues/",
                            "NHL_", year, ".html")
}

years <- seq(2008,2015,1)
# 2008 corresponds to 2007-2008 season
# ... 
# 2013 was a lockout year (shortened season) 
# ...
# 2015 was the last completed season, 2014-2015

for(i in 1:length(years)) {
    year <- years[i]
    season_data <- readHTMLTable(season_url(year)) # convert html tables to data.frame
    season_data_names <- names(season_data)
    season_data_cols <- which(season_data_names == "teams") 
    league_season_data <- season_data[[season_data_cols[1]]][31,] # league aggregate
    season_data <- cbind(season_data[[season_data_cols[1]]][1:30,], 
                         season_data[[season_data_cols[2]]][1:30,]) # team stats
    league_season_data <- cbind(year,league_season_data) # keep track of year
    names(league_season_data)[1] <- "Season" # rename season column
    season_data <- cbind(rep(year,30),season_data) 
    names(season_data)[1] <- "Season"
    if(i==1) {
        all_season_data <- season_data # team stats (all seasons)
        all_league_season_data <- league_season_data # league aggregate (all seasons)
    } else {
        all_season_data <- rbind(all_season_data, season_data)
        all_league_season_data <- rbind(all_league_season_data, league_season_data)
    }
}
all_season_data <- all_season_data[,-c(29)] # Remove junk cols
all_league_season_data <- all_league_season_data[,-c(2,3,13,14,28)]
row.names(all_league_season_data) <- 1:length(all_league_season_data[,1])
tmp <- all_season_data # Merge cols to team name to make sure advanced/ basic metrics line up
for(i in 1:length(years)) {
    j <- ((i-1)*30+1):(i*30)
    if(i==1) {
        all_season_data <- merge(x=tmp[j,1:28], y=tmp[j,29:42], by.x="Team", by.y="Team.1")
    } else {
        all_season_data <- rbind(all_season_data,
            merge(x=tmp[j,1:28], y=tmp[j,29:42], by.x="Team", by.y="Team.1"))
    }
}
# convert cols to numeric
all_league_season_data <-
    data.frame(apply(all_league_season_data, 2, function(x) as.numeric(as.character(x))))
all_season_data[,2:41] <-
    data.frame(apply(all_season_data[,2:41], 2, function(x) as.numeric(as.character(x))))

save(all_league_season_data, file="Data/all_league_season_data.Rdata")
save(all_season_data, file="Data/all_season_data.Rdata")

## Basic metrics
# Rank:     Rank out of 30 teams. Teams with * in name made playoffs
# AvAge:    Average age of time weighted by time on ice
# GP:       Games played
# W:        Wins
# L:        Losses
# OTL:      Overtime Losses
# PTS:      Points *
# PTS%:     Percent of possible points
# GF:       Goals for
# GA:       Goals against
# SRS:      Simple rating system taking into account avg. goal differential and SOS
# SOS:      Strength of schedule demoniminated in goals above/below average
# TG/G:     Total goals per game
# PP:       Power play goals
# PPO:      Power play opportunities
# PP%:      Power play percentage
# PPA:      Power play goals against
# PPOA:     Power play opportunities against
# PK%:      Penalty kill percentage
# SH:       Short-handed goals
# SHA:      Short-hand goals against
# S:        Shots
# S%:       Shot percentage
# SA:       Shots against
# SV%:      Save percentage
# PDO:      PDO at even strength percentage + SV%
 
## Advanced metrics
# CF:       Corsi for in all situations: Shots + Blocks + Misses
# CA:       Corsi against in all situations: Shots + Blocks + Misses
# CF%:      CF / (CF + CA). Above 50% mean team was controlled the puck more often 
# FF:       Fenwick for in all situations: shots + misses
# FA:       Fenwick against in all situations: shots + misses
# oZS%      Offensive Zone F/offs / (Offensive Zone F/offs + Defensive Zone F/offs)
# dZS%      Defensive Zone F/offs / (Offensive Zone F/offs + Defensive Zone F/offs)
# Hits:     Number of hits
# Blocks:   Blocks in all situations
# FOwin:    Faceoffs wins in all situations
# FOloss:   Faceoff losses in all situations
# FO%:      Faceoff win percentage

## Comment on percentages  
# FO%   combines    FOwin, FOloss
# SV%   combines    GA, SA
# S%    combines    GF, S
# PK%   combines    PPA, PPOA
# PP%   combines    PP, PPOA
# PTS%  combines    PTS, W, L