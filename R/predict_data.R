# run get_new_data and load in the appropriate .Rdata file
setwd("~/Documents/Stat350Project")
load("Data/models.Rdata")
pdate <- "2015-11-21"

# <LOAD in the appropriate .Rdata file>
load(paste0("Data/new_season_data",pdate,".Rdata"))
team_names <- as.character(new_season_data$Team)

MSE <- function(actual, expected) {
  mean((actual-expected)^2)
}

actual_pts <- new_season_data$PTS 
prop_games <- mean(new_season_data$GP)/82 # scale some variables by prop_games

# Assess MSE of all models
cmodel2 <- coef(model2)
names(cmodel2)
pmodel2 <- cmodel2[1]*prop_games + cmodel2[2]*new_season_data$GF + 
  cmodel2[3]*new_season_data$GA
mse2 <- MSE(actual_pts,pmodel2)

cmodel4 <- coef(model4)
names(cmodel4)
pmodel4 <- cmodel4[1]*prop_games + cmodel4[2]*new_season_data$GF + 
  cmodel4[3]*new_season_data$GA + cmodel4[4]*new_season_data$S + 
  cmodel4[5]*new_season_data$SA
mse4 <- MSE(actual_pts,pmodel4)

cmodel5 <- coef(model5)
names(cmodel5)
pmodel5 <- cmodel5[1]*prop_games + cmodel5[2]*new_season_data$GF + 
  cmodel5[3]*new_season_data$GA + cmodel5[4]*new_season_data$SOS + 
  cmodel5[5]*new_season_data$S + cmodel5[6]*new_season_data$SA
mse5 <- MSE(actual_pts,pmodel5)

cmodel6 <- coef(model6)
names(cmodel6)
pmodel6 <- cmodel6[1]*prop_games + cmodel6[2]*prop_games*new_season_data$AvAge + 
  cmodel6[3]*new_season_data$GF + cmodel6[4]*new_season_data$GA + 
  cmodel6[5]*new_season_data$SRS*prop_games + cmodel6[6]*new_season_data$S + 
  cmodel6[7]*new_season_data$SA 
mse6 <- MSE(actual_pts,pmodel6)

cmodel7 <- coef(model7)
names(cmodel7)
pmodel7 <- cmodel7[1]*prop_games + cmodel7[2]*prop_games*new_season_data$AvAge + 
  cmodel7[3]*new_season_data$GF + cmodel7[4]*new_season_data$GA + 
  cmodel7[5]*new_season_data$SA + cmodel7[6]*new_season_data$FFper*prop_games + 
  cmodel7[7]*new_season_data$BLK + cmodel7[8]*new_season_data$SOS*prop_games
mse7 <- MSE(actual_pts,pmodel7)

cmodel8 <- coef(model8)
names(cmodel8)
pmodel8 <- cmodel8[1]*prop_games + cmodel8[2]*prop_games*new_season_data$AvAge + 
  cmodel8[3]*new_season_data$GF + cmodel8[4]*new_season_data$GA + 
  cmodel8[5]*new_season_data$SA + cmodel8[6]*new_season_data$FFper*prop_games + 
  cmodel8[7]*new_season_data$BLK + cmodel8[8]*new_season_data$SOS*prop_games + 
  cmodel8[9]*new_season_data$PKper*prop_games
mse8 <- MSE(actual_pts,pmodel8)

cmodel3nl <- coef(model3nl)
names(cmodel3nl)
pmodel3nl <- cmodel3nl[1]*prop_games + cmodel3nl[2]*new_season_data$SRS*prop_games + 
  cmodel3nl[3]*new_season_data$SOS*prop_games + cmodel3nl[4]*new_season_data$SH
mse3nl <- MSE(actual_pts,pmodel3nl)

cmodel4nl <- coef(model4nl)
names(cmodel4nl)
pmodel4nl <- cmodel4nl[1]*prop_games + cmodel4nl[2]*new_season_data$GF + 
  cmodel4nl[3]*new_season_data$GA + cmodel4nl[4]*new_season_data$SH + 
  cmodel4nl[5]*new_season_data$SHA
mse4nl <- MSE(actual_pts, pmodel4nl)

cmodel5nl <- coef(model5nl)
names(cmodel5nl)
pmodel5nl <- cmodel5nl[1]*prop_games + cmodel5nl[2]*new_season_data$GF + 
  cmodel5nl[3]*new_season_data$GA + cmodel5nl[4]*new_season_data$AvAge*prop_games + 
  cmodel5nl[5]*new_season_data$SH + cmodel5nl[6]*new_season_data$SHA
mse5nl <- MSE(actual_pts, pmodel5nl)

save(mse2,mse4,mse5,mse6,mse7,mse8,mse3nl,mse4nl,mse5nl, 
     pmodel2, pmodel4, pmodel5, pmodel6, pmodel7, pmodel8, 
     pmodel3nl, pmodel4nl, pmodel5nl, actual_pts, 
     team_names, file="Data/models_mse.Rdata")
