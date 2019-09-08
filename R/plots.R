library(MASS)
library(scatterplot3d)

library(scatterplot3d)
load("~/Documents/Stat350Project/Data/all_season_data.Rdata")
load("~/Documents/Stat350Project/Data/models.Rdata")
load("~/Documents/Stat350Project/Data/models_mse.Rdata")

# generate plots to use in report
names(all_season_data) <- c("Team", "Season", "Rk", "AvAge", "GP", "W", "L", "OL",
  "PTS", "PTSper", "GF", "GA", "SRS", "SOS", "TGperG", "PP", "PPO",
  "PPper", "PPA", "PPOA", "PKper", "SH", "SHA", "S", "Sper", "SA",
  "SVper", "PDO", "CF", "CA", "CFper", "FF", "FA", "FFper", "oZSper",
  "dZSper", "HIT", "BLK", "FOwin", "FOloss","FOper","team_salary","max_salary")

all_season_data_nl <- all_season_data[c(1:150,181:240),]
s3d_data <- data.frame(GF=as.vector(sample(all_season_data_nl$GF,size = 120)),
                       GA=as.vector(sample(all_season_data_nl$GF,size = 120)),
                       Points=as.vector(sample(all_season_data_nl$PTS,size = 120)))
model <- lm(PTS ~ GF + GA, data=all_season_data_nl)
names(s3d_data) <- c("Goals For", "Goals Against", "Points")
s3d <- scatterplot3d(s3d_data, pch=20, type='p',
                     angle = 220, scale.y = .6,
                     highlight.3d=TRUE, main="",
                     lab=c(3,3,3),zlim=c(45,155),
                     y.margin.add=0.2)
s3d$plane3d(model, lty.box="solid")

names(coef(model4))
par(mar=c(4,4,4,4))
plot(all_season_data$PTS, model4$residuals, xlab="Points", ylab="Residual",
     pch=20)
abline(h=0, col='red', lty=2)

plot(actual_pts, pmodel4-actual_pts, xlab="Points", ylab="Residual",
     pch=20)
abline(h=0, col='red', lty=2)

# plot(all_season_data$GF, model4$residuals); abline(h=0,col='red',lty=2)
# plot(all_season_data$GA, model4$residuals); abline(h=0,col='red',lty=2)
# plot(all_season_data$S, model4$residuals); abline(h=0,col='red',lty=2)
# plot(all_season_data$SA, model4$residuals); abline(h=0,col='red',lty=2)
