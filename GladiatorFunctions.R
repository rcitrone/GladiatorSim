library(dplyr)
library(ggplot2)

names <- read.csv("names.csv")
firstnames <- as.character(unique(names$firstname))
lastnames <- as.character(unique(names$lastname))
lastnames <- gsub("\\?", "", x=lastnames)

### Generating Fighters
genFighter <- function(startID=1, n=1, year=1, mean.val = 500, sd.val = 120, coe.sd = .05){
  physical.element <- rnorm(n=n, mean=mean.val*0.3, sd = sd.val*0.3)
  mental.element <- rnorm(n=n, mean=0, sd = sd.val*0.2)
  st.base <- rnorm(n=n, mean = mean.val*0.7, sd = sd.val*0.7) + physical.element
  sp.base <- rnorm(n=n, mean = mean.val*0.7, sd = sd.val*0.7) + physical.element
  wt.base <- rnorm(n=n, mean = mean.val, sd = sd.val*0.8) + mental.element
  ag.base <- rnorm(n=n, mean = mean.val, sd = sd.val) - mental.element
  barracks <- data.frame('playerID' = c(startID:(startID+n-1)), 'firstname' = sample(x = firstnames, size = n , replace = TRUE), 'lastname' = sample(x=lastnames, size = n, replace = TRUE), 'born' = rep(year, n), 'skill.base' = rnorm(n=n, mean=mean.val, sd = sd.val), 'strength.base' = st.base, 'speed.base' = sp.base , 'wit.base' = wt.base, 'aggression.base' = ag.base, 'skill.coe' = rnorm(n=n, mean=0, sd = coe.sd), 'speed.coe' = rnorm(n=n, mean=0, sd = coe.sd), 'strength.coe' = rnorm(n=n, mean=0, sd = coe.sd) , 'wit.coe' = rnorm(n=n, mean=0, sd = coe.sd))
  return(barracks)
}


#barracks <- data.frame('playerID' = c(1:10), 'born' = 1, 'skill.base' = rnorm(n=10, mean=500, sd = 120), 'strength.base' = rnorm(n=10, mean=500, sd = 120), 'wit.base' = rnorm(n=10, mean=500, sd = 120), 'speed.base' = rnorm(n=10, mean=500, sd = 120), 'aggression.base' = rnorm(n=10, mean=500, sd = 120))

#### Aging Curves
# sp.age <- c(seq(from = 0.8, to = 1, by = 0.04), rep(1, 5), seq(from = 1, to = 0.8, by = -0.03), seq(from = 0.8, to = 0.5, by = -0.01))
# st.age <- c(seq(from = 0.7, to = 1, by = 0.05), rep(1, 5), seq(from = 1, to = 0.8, by = -0.02), seq(from = 0.8, to = 0.4, by = -0.02))
# wit.age <- c(seq(from = 0.5, to = 0.9, by = 0.08), seq(from = 0.9, to = 1, by = 0.02), rep(1, 2), seq(from = 1, to = 1.3, by = 0.01))
# sk.age <- c(seq(from = 0.5, to = 0.9, by = 0.05), seq(from = 0.9, to = 1, by = 0.02), rep(1, 8), seq(from = 1, to = 0.8, by = -0.01))
# age <- c(15:(15+43))
# sp.age <- sp.age[1:44]
# wit.age <- wit.age[1:44]
# age.curve <- as.data.frame(cbind(age, sp.age, st.age, sk.age, wit.age))
#write.csv(age.curve, "agecurve.csv" )
age.curve <- read.csv("agecurve.csv")
ggplot(age.curve, aes(x=age)) + geom_line(aes(y=sp.age), col = "green") + geom_line(aes(y=st.age), col = "red") + geom_line(aes(y=sk.age), col = "orange") + geom_line(aes(y=wit.age), col = "blue")


### Aging Function
ageFighter <- function(df, newyear, ac =age.curve, coe.sd = 0.02){
  df$age <- newyear - df$born
  df$skill.coe <- df$skill.coe + rnorm(n=nrow(df), mean=0, sd = (coe.sd*1.1))
  df$speed.coe <- df$speed.coe + rnorm(n=nrow(df), mean=0, sd = coe.sd)
  df$strength.coe <- df$strength.coe + rnorm(n=nrow(df), mean=0, sd = coe.sd)
  df$wit.coe <- df$wit.coe + rnorm(n=nrow(df), mean= 0, sd = (coe.sd*1.1))
  df.full <- left_join(df, ac, by = "age")
  df$strength <- df$strength.base * (df.full$st.age + df.full$strength.coe)
  df$speed <- df$speed.base * (df.full$sp.age + df.full$speed.coe)
  df$wit <- df$wit.base * (df.full$wit.age + df.full$wit.coe)
  df$skill <- df$skill.base * (df.full$sk.age + df.full$skill.coe)
  df$aggression <- df$aggression.base
  return(df)
}



### Battle Function
fight <- function(home, away, df, rand.mean = 500, rand.sd = 200){
  hdf <- subset(df, playerID == home)
  adf <- subset(df, playerID == away)
  hdf2 <- as.vector(hdf[,c("skill", "strength", "wit", "speed")])
  adf2 <- as.vector(adf[,c("skill", "strength", "wit", "speed")])
  hdf2 <- hdf2[1,]
  adf2 <- adf2[1,]
  diff.scores <- hdf2/adf2
  rand.vals <- rnorm(4, mean= rand.mean, sd = rand.sd)
  total.scores <- as.integer(diff.scores * rand.vals)
  winner <- ifelse(mean(total.scores) > rand.mean, home, away)
  loser <- ifelse(winner == home, away, home)
  return(c("winner" = winner, "loser" = loser))
}


### Season Battle
seasonSim <- function(df, battle.log = battle.log, matchlevel = 1, seasonnum = 1){
  p <- nrow(df)
  df$preserveID <- df$playerID
  df$playerIDnew <- c(1:p)
  bl <- battle.log[1,]
  for(jj in 1:(p-1)){
    for(kk in (jj+1):p){
      home.playerID <- df$preserveID[df$playerIDnew == jj]
      away.playerID <- df$preserveID[df$playerIDnew == kk]
      result <- fight(home.playerID, away.playerID, df)
      result.frame <- data.frame("weeknum" = jj, "winnerID" = result[1], "loserID" = result[2], "matchlevel" = matchlevel, "seasonnum"= seasonnum)
      bl <- rbind(bl, result.frame)
    }
  }
  bl <- bl[-1,]
  bl$rand <- runif(nrow(bl), min = 0, max = 1)
  bl <- bl[order(bl$rand),]
  bl$weeknum <- c(1:nrow(bl))
  bl <-  bl[, !names(bl) %in% c("rand")]
  battle.log <- rbind(battle.log, bl)
  return(battle.log)
}


### 16-person Tournament
seeding <- read.csv('seeding.csv')
tourney16 <- function(df, battle.log = battle.log, seasonnum){
  winners <- c(1:15)
  for(pp in 1:nrow(seeding)){
    hp <- match(seeding$home[pp], df$seed)
    ap <- match(seeding$away[pp], df$seed)
    home.playerID <- df$playerID[hp]
    away.playerID <- df$playerID[ap]
    result <- fight(home.playerID, away.playerID, df)
    result.frame <- data.frame("weeknum" = 1, "winnerID" = result[1], "loserID" = result[2], 'matchlevel' = "Round 1", seasonnum = seasonnum)
    battle.log <- rbind(battle.log, result.frame)
    winners[pp] <- result[1]
  }
  r2 <- seq(from=1, to=7, by=2) 
  battlenum <- 9
  for(pp in r2){
    result <- fight(winners[pp], winners[pp + 1], df)
    result.frame <- data.frame("weeknum" = 2, "winnerID" = result[1], "loserID" = result[2], 'matchlevel' = "Round 2", seasonnum = seasonnum)
    battle.log <- rbind(battle.log, result.frame)
    winners[battlenum] <- result[1]
    battlenum <- battlenum + 1
  }
  semi1 <- fight(winners[9], winners[10], df)
  semi2 <- fight(winners[11], winners[12], df)
  result.frame1 <- data.frame("weeknum" = 3, "winnerID" = semi1[1], "loserID" = semi1[2], 'matchlevel' = "Round 3", seasonnum = seasonnum)
  result.frame2 <- data.frame("weeknum" = 3, "winnerID" = semi2[1], "loserID" = semi2[2], 'matchlevel' = "Round 3", seasonnum = seasonnum)
  battle.log <- rbind(battle.log, result.frame1, result.frame2)
  final <- fight(semi1[1], semi2[1], df)
  result.final <- data.frame("weeknum" = 4, "winnerID" = final[1], "loserID" = final[2], 'matchlevel' = "Round 4" , seasonnum = seasonnum)
  battle.log <- rbind(battle.log, result.final)
  return(battle.log)
}


#### Kings Tourney
ktseed <- read.csv("ktseed.csv")
kingstourney <- function(df, battle.log = battle.log, seasonnum){
  df <- df[order(df$ELO, decreasing = TRUE),]
  df$ovseed <- c(1:nrow(df))
  df.short <- subset(df, ovseed <= 64)
  df.short <- left_join(df.short, ktseed, by = "ovseed")
  for(ll in 1:4){
    df.limit <- subset(df.short, group == ll)
    battle.log <- tourney16(df.limit, battle.log = battle.log, seasonnum = seasonnum)
  }
  finalfour <- battle.log$winnerID[battle.log$matchlevel == "Round 4" & battle.log$seasonnum == seasonnum]
  print(finalfour)
  finalist1 <- fight(finalfour[1], finalfour[2], df)
  finalist2 <- fight(finalfour[3], finalfour[4], df)
  result.frame1 <- data.frame("weeknum" = 5, "winnerID" = finalist1[1], "loserID" = finalist1[2], 'matchlevel' = "Semifinal", seasonnum = seasonnum)
  result.frame2 <- data.frame("weeknum" = 5, "winnerID" = finalist2[1], "loserID" = finalist2[2], 'matchlevel' = "Semifinal", seasonnum = seasonnum)
  ktfinal <- fight(finalist1[1], finalist2[1], df)
  result.final <- data.frame("weeknum" = 6, "winnerID" = ktfinal[1], "loserID" = ktfinal[2], 'matchlevel' = "Final", seasonnum = seasonnum)
  battle.log <- rbind(battle.log, result.frame1, result.frame2, result.final)
  return(battle.log)
}

league.seeding <- c(rep("S", 25), rep(c("A1", "A2"), 25), rep(c("B1", "B2"), 25), rep(c("J1", "J2"), 25))


runHistory <- function(df, battle.log, df.history, startyear=51, yearstorun=10, nextid=176){
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
    if(nrow(battle.log) > 5000){
      startindex <- nrow(battle.log) - 5000 - 1
      bl <- battle.log[startindex:nrow(battle.log),]
    } else { bl <- battle.log }
    bl$order <- c(1:nrow(bl))
    elo.date <- as.Date("2017-01-01") + bl$order
    winners <- bl$winnerID
    losers <- bl$loserID
    elo.calc <- elo.seq(winner = winners, loser = losers, Date = elo.date, init = "bottom", k = 40, runcheck = FALSE)
    eg <- elo.calc$cmat
    eglim <- as.data.frame(eg[nrow(eg),])
    eglim$winnerID <- rownames(eglim)
    eglim$winnerID <- as.numeric(eglim$winnerID)
    colnames(eglim) <- c("ELO", "playerID")
    drop <- c("ELO")
    df <- df[,!(names(df) %in% drop)]
    df <- left_join(df, eglim, by = "playerID")
    
    #Kings tourney
    battle.log <- kingstourney(df, battle.log = battle.log, seasonnum = jj)
    df <- df[order(df$ELO, decreasing = TRUE),]
    df.history <- rbind(df.history, df)
    df$league <- league.seeding
    df <- subset(df, (age <= 20 | !(league %in% c("J1", "J2"))) & ELO > 550)
    numnew <- 175-nrow(df)
    if(numnew > 0){
      new.recruits <- genFighter(startID = current.id, n=numnew, year= jj-15)
      current.id <- current.id + numnew
      new.recruits <- ageFighter(new.recruits, newyear = jj)
      num.j1 <- 25 - nrow(subset(df, league == "J1"))
      new.league <- c(rep("J1", num.j1), rep("J2", numnew - num.j1))
      new.recruits$rand <- runif(nrow(new.recruits), min = 0, max = 1)
      new.recruits$league <- new.league
      new.recruits$ELO <- 700
      df <- rbind(df, new.recruits)
    }
    print(jj)
  }
  output <- c(df, battle.log, df.history)
  return(output)
}
