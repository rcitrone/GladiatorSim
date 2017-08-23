source("GladiatorFunctions.R")
library(ggthemes)
library(ggplot2)
library(EloRating)

roster.x <- read.table("roster.csv")
battle.log.x <- read.table("blx.csv")
rh2<- read.table("roster.history.csv")
#write.table(roster.x, "roster.csv")
#write.table(battle.log.x, "blx.csv")
#write.table(roster.history, "rosterhistory.csv")

######################## Running Years ##################################33
year.next <- max(roster.x$born + roster.x$age) + 1
test <- runHistory(roster.x, bl.new, rh2, startyear = year.next, yearstorun = 25, nextid = (max(roster.x$playerID) + 1))

roster.x <- as.data.frame(test[1:22])
battle.log.x <- as.data.frame(test[23:27])
rh2 <- as.data.frame(test[28:49])

roster.history <- rh2

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
    kt.semifinal = sum(sumcol[matchlevel %in% c("Round 4")]),
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
