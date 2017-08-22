source("GladiatorFunctions.R")
library(ggthemes)
library(ggplot2)
library(EloRating)



### Initial Player Generation ###
battle.log <- data.frame("weeknum" = 1, "winnerID" = 1, "loserID" = 1, "matchlevel" = 1, "seasonnum" = 1)
years <- c(16:50)
dirty100 <- genFighter(n=175)
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
roster$born <- 50 - roster$age
## Placement Season
roster$league <- sample(c(rep(1, 25), rep(2, 25), rep(3, 25), rep(4, 25), rep(5, 25), rep(6, 25), rep(7, 25)), size=nrow(roster), replace = FALSE)
#battle.log <- data.frame("weeknum" = 1, "winnerID" = 1, "loserID" = 1, "matchlevel" = 1, "seasonnum" = 1)
for(qq in 1:7) {
  section7 <- subset(roster, league == qq)
  battle.log <- seasonSim(section7, battle.log = battle.log, matchlevel = qq, seasonnum = 50)
  print(qq)
}
battle.log <- battle.log[2:nrow(battle.log),]

#Calculating ELO
bl <- battle.log
bl$sumcol <- 1
bl$order <- c(1:nrow(bl))
elo.date <- as.Date("2017-01-01") + bl$order
winners <- bl$winnerID
losers <- bl$loserID
elo.calc <- elo.seq(winner = winners, loser = losers, Date = elo.date, init = "average", k = 65, runcheck = FALSE)
eg <- elo.calc$cmat
eglim <- as.data.frame(eg[nrow(eg),])
eglim$playerID <- rownames(eglim)
eglim$playerID <- as.numeric(eglim$playerID)
colnames(eglim) <- c("ELO", "playerID")
roster.full <- left_join(roster, eglim, by = "playerID")
#First Kings Tourney
battle.log <- kingstourney(roster.full,  battle.log = battle.log ,50 )

league.seeding <- c(rep("S", 25), rep(c("A1", "A2"), 25), rep(c("B1", "B2"), 25), rep(c("J1", "J2"), 25))
roster.full <- roster.full[order(roster.full$ELO, decreasing = TRUE),]
roster.full$league <- league.seeding

test <- runHistory(roster.full, battle.log, roster.full, startyear = 51, yearstorun = 20)
roster.x <- as.data.frame(test3[1:22])
battle.log.x <- as.data.frame(test3[23:27])
roster.history <- as.data.frame(test3[28:49])
year.next <- max(roster.x$born + roster.x$age) + 1

test3 <- runHistory(roster.x, battle.log.x, roster.history, startyear = year.next, yearstorun = 30, nextid = (max(roster.x$playerID) + 1))

#write.table(roster.x, "roster.csv")
#write.table(battle.log.x, "blx.csv")
#write.table(roster.history, "rosterhistory.csv")

bl <- battle.log.x
bl$sumcol <- 1
standing <- bl %>% group_by(winnerID) %>%
  summarize(
    seasons = length(unique(seasonnum)),
    total.wins =  sum(sumcol),
    S.wins = sum(sumcol[matchlevel == "S"]),
    A.wins = sum(sumcol[matchlevel %in% c("A1", "A2")]),
    B.wins = sum(sumcol[matchlevel %in% c("B1", "B2")]),
    J.wins = sum(sumcol[matchlevel %in% c("J1", "J2")]),
    kt.wins = sum(sumcol[matchlevel == "Final"]),
    last.season = max(seasonnum)
  )
standing.loss <- bl %>% group_by(loserID) %>%
  summarize(
    total.losses =  sum(sumcol),
    S.loss = sum(sumcol[matchlevel == "S"]),
    A.loss = sum(sumcol[matchlevel %in% c("A1", "A2")]),
    B.loss = sum(sumcol[matchlevel %in% c("B1", "B2")]),
    J.loss = sum(sumcol[matchlevel %in% c("J1", "J2")])
  )
standing.loss$winnerID <- standing.loss$loserID
standing <- left_join(standing, standing.loss, by = "winnerID")
standing$total.fights <- standing$total.wins + standing$total.losses

roster.history <- roster.history[order(roster.history$age, decreasing = TRUE),]
all.players <- roster.history[!duplicated(roster.history$playerID),]
standing$playerID <- standing$winnerID
standing.full <- left_join(standing, all.players, by = "playerID")

roster.history$seasonnum <- roster.history$born + roster.history$age
standing.year <- bl %>% group_by(winnerID, seasonnum) %>%
  summarize(
    total.wins =  sum(sumcol),
    S.wins = sum(sumcol[matchlevel == "S"]),
    A.wins = sum(sumcol[matchlevel %in% c("A1", "A2")]),
    B.wins = sum(sumcol[matchlevel %in% c("B1", "B2")]),
    J.wins = sum(sumcol[matchlevel %in% c("J1", "J2")]),
    kt.wins = sum(sumcol[matchlevel == "Final"])
  )
standing.lossyear <- bl %>% group_by(loserID, seasonnum) %>%
  summarize(
    total.loss =  sum(sumcol),
    S.loss = sum(sumcol[matchlevel == "S"]),
    A.loss = sum(sumcol[matchlevel %in% c("A1", "A2")]),
    B.loss = sum(sumcol[matchlevel %in% c("B1", "B2")]),
    J.loss = sum(sumcol[matchlevel %in% c("J1", "J2")])
  )
standing.lossyear$merge <- paste(standing.lossyear$loserID, standing.lossyear$seasonnum, sep = ",")

roster.history <- roster.history[order(roster.history$age, decreasing = TRUE),]
roster.history$merge <- paste(roster.history$playerID, roster.history$seasonnum, sep = ",")

standing.year$merge <- paste(standing.year$winnerID, standing.year$seasonnum, sep = ",")
standing.year <- left_join(standing.year, standing.lossyear, by = "merge")
history.full <- left_join(roster.history, standing.year, by = "merge")
history.full$total.ability <- history.full$skill + history.full$speed + history.full$wit + history.full$strength

### Fun w/ History.full
s83 <- subset(history.full, seasonnum == 83)
slynt <- subset(history.full, playerID %in% c(115, 185))
ggplot(slynt, aes(x=seasonnum, y=total.ability, colour = lastname)) + geom_line() + geom_point(aes(size = kt.wins))
kt.winners <- subset(history.full, kt.wins == 1)
ggplot(kt.winners, aes(x=seasonnum, y=age)) + geom_line()
### Plotting
best <- standing$winnerID[standing$kt.wins >= 1]
roster.best <- subset(history.full, playerID %in% best)
roster.rest <- subset(history.full, !(playerID %in% best))
ggplot(roster.best, aes(x=age, y= total.ability, by = playerID)) + geom_line(aes()) + geom_point(aes( size = kt.wins))

ggplot(roster.best, aes(x= age, y= ELO, by = playerID)) + geom_line(aes(colour = as.factor(lastname))) + geom_point(aes(colour = as.factor(playerID), size = kt.wins))

#tourneymatchup Slynt - Qorgyle
bl.best <- subset(battle.log.x, winnerID %in% c(115, 185) & loserID %in% c(115,185))
length(bl.best$winnerID[bl.best$winnerID == 185])
