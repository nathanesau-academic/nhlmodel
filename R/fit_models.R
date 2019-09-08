library(leaps)

setwd("~/Documents/Stat350Project")
load("Data/all_season_data.Rdata")

names(all_season_data) <- c("Team", "Season", "Rk", "AvAge", "GP", "W", "L", "OL",
  "PTS", "PTSper", "GF", "GA", "SRS", "SOS", "TGperG", "PP", "PPO",
  "PPper", "PPA", "PPOA", "PKper", "SH", "SHA", "S", "Sper", "SA",
  "SVper", "PDO", "CF", "CA", "CFper", "FF", "FA", "FFper", "oZSper",
  "dZSper", "HIT", "BLK", "FOwin", "FOloss","FOper","team_salary","max_salary")

model2 <- lm(PTS ~ GF + GA, data=all_season_data) # baseline model; not based on all.sub

# regsubsets
all.sub <- regsubsets(x=all_season_data[,c(4,11,12,13,14,18,21,22,23,24,26,34,37,38,41)], 
                      y=all_season_data[,9], method = "exhaustive")

summary(all.sub)$which

# with lockout 
model4 <- lm(PTS ~ GF + GA + S + SA, data=all_season_data)
model5 <- lm(PTS ~ GF + GA + SOS + S + SA, data=all_season_data)
model6 <- lm(PTS ~ AvAge + GF + GA + SRS + S + SA, data=all_season_data)
model7 <- lm(PTS ~ AvAge + GF + GA + SA + FFper + BLK + SOS, data=all_season_data)
model8 <- lm(PTS ~ AvAge + GF + GA + SA + FFper + BLK + SOS + PKper, data=all_season_data) # remember

all_season_data_nl <- all_season_data[-c(151:180),]
all.sub <- regsubsets(x=all_season_data_nl[,c(4,11,12,13,14,18,21,22,23,24,26,34,37,38,41)], 
                      y=all_season_data_nl[,9],method = "exhaustive")
summary(all.sub)$which

# without lockout year
model3nl <- lm(PTS ~ SRS + SOS + SH, data=all_season_data_nl)
model4nl <- lm(PTS ~ GF + GA + SH + SHA, data=all_season_data_nl)
model5nl <- lm(PTS ~ GF + GA + AvAge + SH + SHA, data=all_season_data_nl)

modelint <- lm(PTS ~ GF + GA + S*SA, data=all_season_data)
save(model2,model4,model5,model6,model7,model8,model3nl,
     model4nl,model5nl,modelint,file="Data/models.Rdata")