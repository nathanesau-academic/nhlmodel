# Get salary data from http://stats.nhlnumbers.com
library(XML)

setwd("~/Documents/Stat350Project")
options(warn=-1)

teams <- c("ANA","ARI","BOS","BUF",
  "CGY","CAR","CHI","COL",
  "CLB","DAL","DET","EDM",
  "FLA","LAK","MIN","MTL",
  "NAS","NJD","NYI","NYR",
  "OTT","PHI","PIT","SJS",
  "STL","TBL","TOR","VAN",
  "WAS","WPG")

team_names_2008 <- c("ANA","PHX","BOS","BUF",
  "CAL","CAR","CHI","CAV",
  "CLB","DAL","DET","EDM",
  "FLA","LAK","MIN","MTL",
  "NAS","NJD","NYI","NYR",
  "OTT","PHI","PIT","SJS",
  "STL","TBL","TOR","VAN",
  "WAS","ATL")
team_names_2009 <- team_names_2008
team_names_2010 <- team_names_2008
team_names_2011 <- team_names_2008
team_names_2012 <- c("ANA","PHX","BOS","BUF",
  "CAL","CAR","CHI","CAV",
  "CLB","DAL","DET","EDM",
  "FLA","LAK","MIN","MTL",
  "NAS","NJD","NYI","NYR",
  "OTT","PHI","PIT","SJS",
  "STL","TBL","TOR","VAN",
  "WAS","WPG")
team_names_2013 <- team_names_2012
team_names_2014 <- team_names_2012
team_names_2015 <- teams
team_names_2016 <- teams

years <- c(2008,2009,2010,2011,2012,
         2013,2014,2015,2016)

salary_data <- as.data.frame(setNames(replicate(4, numeric(0), simplify=F), 
  c("Team","Year","team_salary","max_salary")))

for(i in 1:length(teams)) {
    for(j in 1:length(years)) {
        tmp <- readHTMLTable(paste0("http://stats.nhlnumbers.com/teams/",
                                    teams[i],"?","year=",years[j]))[[1]]
        tmp <- tmp[which((as.character(tmp[,1]) == "Defencemen") + 
                  (as.character(tmp[,1]) == "Forwards") + 
                  (as.character(tmp[,1]) == "Goaltenders") +
                  (as.character(tmp[,1]) == "Bonus Cushion") +
                  (as.character(tmp[,1]) == "LTIR Replacement") +
                  (as.character(tmp[,1]) == "Cap Space") + 
                  (as.character(tmp[,1]) == "Average Age") +
                  (as.character(tmp[,1]) == "Other") +
                  (as.character(tmp[,1]) == "Inactive") ==0),]
        tmp <- tmp[-c(1,2),]
        tmp[,2] <- as.numeric(as.character(tmp[,2]))
        team_salary <- max(tmp[,2])
        tmp <- tmp[-which(tmp[,2]==max(tmp[,2])),]
        tmp_df  <- data.frame(Team=teams[i], Year=years[j], 
                              team_salary=team_salary,
                              max_salary=sum(sort(tmp[,2], T)[1:2]))
        salary_data <- rbind(salary_data, tmp_df)
    }
}

# sort by year and then alphabetically
salary_data <- salary_data[with(salary_data, order(Year,Team)), ]
salary_data$Team <- as.character(salary_data$Team)
salary_data$Team[1:30] <- team_names_2008
salary_data$Team[31:60] <- team_names_2009
salary_data$Team[61:90] <- team_names_2010
salary_data$Team[91:120] <- team_names_2011 
salary_data$Team[121:150] <- team_names_2012 
salary_data$Team[151:180] <- team_names_2013 
salary_data$Team[181:210] <- team_names_2014 
salary_data$Team[211:240] <- team_names_2015 
salary_data$Team[241:270] <- team_names_2016
salary_data <- salary_data[with(salary_data, order(Year,Team)), ]
save(salary_data,file="Data/salary_data.Rdata")