source("GladiatorFunctions.R")
library(ggthemes)
library(ggplot2)
library(EloRating)
battle.log <- data.frame("weeknum" = 1, "winnerID" = 1, "loserID" = 1, "matchlevel" = 1, "seasonnum" = 1)



### Initial Player Generation ###
years <- c(16:60)
dirty100 <- genFighter(n=210)
dirty100 <- ageFighter(dirty100, newyear=16)
dcareer <- dirty100
for(jj in 1:length(years)) {
  yr <- years[jj]
  dirty100 <- ageFighter(dirty100, newyear=yr)
  dcareer <- rbind(dcareer, dirty100)
}
dcareer$rand <- runif(n=nrow(dcareer), min=0, max=1)
dcareer <- dcareer[order(dcareer$rand),]
roster <- dcareer[!duplicated(dcareer$playerID),]
## Placement Season
roster$league <- sample(c(rep(1, 30), rep(2, 30), rep(3, 30), rep(4, 30), rep(5, 30), rep(6, 30), rep(7, 30)), size=nrow(roster), replace = FALSE)
#battle.log <- data.frame("weeknum" = 1, "winnerID" = 1, "loserID" = 1, "matchlevel" = 1, "seasonnum" = 1)
for(qq in 1:7) {
  section7 <- subset(roster, league == qq)
  battle.log <- seasonSim(section7, battle.log = battle.log, matchlevel = qq, seasonnum = 1)
  print(qq)
}

bl <- battle.log
#Generating Standings
bl$sumcol <- 1
standing <- bl %>%
  group_by(winnerID , seasonnum, matchlevel) %>%
  summarize( wins = sum(sumcol[winnerID == winnerID],
                        kt.titles = sum(sumcol[winnerID == winnerID & matchlevel == "Final"])))
standing$playerID <- standing$winnerID
#Calculating ELO
bl$order <- c(1:nrow(bl))
elo.date <- as.Date("2017-01-01") + bl$order
winners <- battle.log$winnerID
losers <- battle.log$loserID
elo.calc <- elo.seq(winner = winners, loser = losers, Date = elo.date, init = "average", k = 65, runcheck = FALSE)
eg <- elo.calc$cmat
eglim <- as.data.frame(eg[nrow(eg),])
eglim$winnerID <- rownames(eglim)
eglim$winnerID <- as.numeric(eglim$winnerID)
standing <- left_join(standing, eglim, by = "winnerID", copy = TRUE)
colnames(standing) <- c("winnerID", "seasonnum", "matchlevel", "wins", "kt.wins", "ELO")
standing <- standing[order(standing$ELO, decreasing = TRUE),]
standing$seed <- c(1:nrow(standing))
standing$playerID <- standing$winnerID
roster.full <- left_join(roster, standing, by = "playerID")
#First Kings Tourney
battle.log <- kingstourney(roster.full,  battle.log = battle.log , 1, 1 )

### Do it again!
bl <- battle.log
#Generating Standings
bl$sumcol <- 1
standing <- bl %>%
  group_by(winnerID , seasonnum, matchlevel) %>%
  summarize( wins = sum(sumcol[winnerID == winnerID],
                        kt.titles = sum(simcol[winnerID == winnerID & matchlevel == "Final"])))
standing$playerID <- standing$winnerID
standing <- left_join(standing, roster, by = "playerID")
#Calculating ELO
bl$order <- c(1:nrow(bl))
elo.date <- as.Date("2017-01-01") + bl$order
winners <- battle.log$winnerID
losers <- battle.log$loserID
elo.calc <- elo.seq(winner = winners, loser = losers, Date = elo.date, init = "average", k = 65, runcheck = FALSE)
eg <- elo.calc$cmat
eglim <- as.data.frame(eg[nrow(eg),])
eglim$winnerID <- rownames(eglim)
eglim$winnerID <- as.numeric(eglim$winnerID)
standing <- left_join(standing, eglim, by = "winnerID", copy = TRUE)
colnames(standing) <- c("winnerID", "seasonnum", "matchlevel", "wins", "kt.wins", "ELO")
standing <- standing[order(standing$ELO, decreasing = TRUE),]
standing$seed <- c(1:nrow(standing))
roster.full <- left_join(roster, standing, by = "playerID")
