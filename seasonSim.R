source("GladiatorFunctions.R")
library(ggthemes)
library(ggplot2)
library(EloRating)



### Initial Player Generation ###
battle.log <- data.frame("weeknum" = 1, "winnerID" = 1, "loserID" = 1, "matchlevel" = 1, "seasonnum" = 1)
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
roster$born <- 50 - roster$age
## Placement Season
roster$league <- sample(c(rep(1, 30), rep(2, 30), rep(3, 30), rep(4, 30), rep(5, 30), rep(6, 30), rep(7, 30)), size=nrow(roster), replace = FALSE)
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
battle.log <- kingstourney(roster.full,  battle.log = battle.log ,50, 1 )

league.seeding <- c(rep("S", 30), rep(c("A1", "A2"), 30), rep(c("B1", "B2"), 30), rep(c("J1", "J2"), 30))
roster.full <- roster.full[order(roster.full$ELO, decreasing = TRUE),]
roster.full$league <- league.seeding
leaguelist <- unique(roster.full$league)

runHistory <- function(df, battle.log, df.history, startyear=51, yearstorun=10, nextid=211){
  #age fighters
  current.id = nextid
  for(jj in startyear:(startyear+yearstorun)){
    # age fighters
    df <- ageFighter(df, newyear = jj)
    # Run Season
    for(qq in leaguelist){
      df.subset <- subset(df, league == qq)
      battle.log <- seasonSim(df.subset, battle.log = battle.log, matchlevel = qq, seasonnum = jj)
    }
    
    ## Calculting ELO
    if(nrow(battle.log) > 8000){
      startindex <- nrow(battle.log) - 8000 - 1
      bl <- battle.log[startindex:nrow(battle.log),]
    } else { bl <- battle.log }
    bl$order <- c(1:nrow(bl))
    elo.date <- as.Date("2017-01-01") + bl$order
    winners <- bl$winnerID
    losers <- bl$loserID
    elo.calc <- elo.seq(winner = winners, loser = losers, Date = elo.date, init = "average", k = 50, runcheck = FALSE)
    eg <- elo.calc$cmat
    eglim <- as.data.frame(eg[nrow(eg),])
    eglim$winnerID <- rownames(eglim)
    eglim$winnerID <- as.numeric(eglim$winnerID)
    colnames(eglim) <- c("ELO", "playerID")
    drop <- c("ELO")
    df <- df[,!(names(df) %in% drop)]
    df <- left_join(df, eglim, by = "playerID")
    
    #Kings tourney
    battle.log <- kingstourney(df, battle.log = battle.log, seasonnum = jj, region = 1)
    df <- df[order(df$ELO, decreasing = TRUE),]
    df.history <- rbind(df.history, df)
    df$league <- leaguelist
    df <- subset(df, (age <= 20 | !(league %in% c("J1", "J2"))) & ELO > 550)
    numnew <- 210-nrow(df)
    new.recruits <- genFighter(startID = current.id, n=numnew, year= jj-15)
    current.id <- current.id + numnew
    new.recruits <- ageFighter(new.recruits, newyear = jj)
    num.j1 <- 30 - nrow(subset(df, league == "J1"))
    new.league <- c(rep("J1", num.j1), rep("J2", numnew - num.j1))
    new.recruits$league <- new.league
    new.recruits$ELO <- 700
    df <- rbind(df, new.recruits)
    print(jj)
  }
  output <- c(df, batte.log, df.history)
  return(output)
}

test <- runHistory(roster.full, battle.log, roster.full, startyear = 51, yearstorun = 10)
