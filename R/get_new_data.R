# Read 2015-2016 NHL season data (in progress) from hockey-reference.com
library(XML)

setwd("~/Documents/Stat350Project/")

season_url <- function(year="2016") { # site is hockey-reference.com
  paste0("http://www.hockey-reference.com/leagues/",
         "NHL_", year, ".html")
}

year <- 2016 # read in data from this year
new_season_data <- readHTMLTable(season_url(year)) # convert html tables to data.frame
new_season_data_names <- names(new_season_data)
new_season_data_cols <- which(new_season_data_names == "teams") 
new_league_season_data <- new_season_data[[new_season_data_cols[1]]][31,] # league aggregate
new_season_data <- cbind(new_season_data[[new_season_data_cols[1]]][1:30,], 
                     new_season_data[[new_season_data_cols[2]]][1:30,]) # team stats
new_league_season_data <- cbind(year,new_league_season_data) # keep track of year
names(new_league_season_data)[1] <- "Season" # rename season column
new_season_data <- cbind(rep(year,30),new_season_data) 
names(new_season_data)[1] <- "Season"
new_season_data <- new_season_data[,-c(29)] # Remove junk cols
new_league_season_data <- new_league_season_data[,-c(2,3,13,14,28)]
tmp <- new_season_data # Merge cols to team name to make sure advanced/ basic metrics line up

j <- 1:30
new_season_data <- merge(x=tmp[j,1:28], y=tmp[j,29:42], by.x="Team", by.y="Team.1")

# convert cols to numeric
for(i in 1:23) new_league_season_data[,i] <- as.numeric(as.character(new_league_season_data[,i]))
row.names(new_league_season_data) <- 1
new_season_data[,2:41] <-
  data.frame(apply(new_season_data[,2:41], 2, function(x) as.numeric(as.character(x))))

names(new_season_data) <- c("Team", "Season", "Rk", "AvAge", "GP", "W", "L", "OL",
                        "PTS", "PTSper", "GF", "GA", "SRS", "SOS", "TGperG", "PP", "PPO",
                        "PPper", "PPA", "PPOA", "PKper", "SH", "SHA", "S", "Sper", "SA",
                        "SVper", "PDO", "CF", "CA", "CFper", "FF", "FA", "FFper", "oZSper",
                        "dZSper", "HIT", "BLK", "FOwin", "FOloss","FOper")

save(new_season_data,file=paste0("Data/new_season_data",Sys.Date(),".Rdata"))