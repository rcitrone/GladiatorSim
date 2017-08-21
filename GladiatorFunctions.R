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
tourney16 <- function(df, battle.log = battle.log, seasonnum = 1, region = 1){
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
kingstourney <- function(df, battel.log = battle.log, seasonnum = 1, region = 1){
  df <- df[order(df$ELO, decreasing = TRUE),]
  df$ovseed <- c(1:nrow(df))
  df <- subset(df, ovseed <= 64)
  df <- left_join(df, ktseed, by = "seed")
  for(ll in 1:4){
    df.limit <- subset(df, group == 1)
    battle.log <- tourney16(df.limit, battle.log = battle.log, seasonnum = seasonnum, region = region)
  }
  finalfour <- battle.log$winnerID[battle.log$matchlevel == "Round 4" & seasonnum == seasonnum]
  finalist1 <- fight(finalfour[1], finalfour[2], df)
  finalist2 <- fight(finalfour[3], finalfour[4], df)
  result.frame1 <- data.frame("weeknum" = 5, "winnerID" = finalist1[1], "loserID" = finalist1[2], 'matchlevel' = "Semifinal", seasonnum = seasonnum)
  result.frame2 <- data.frame("weeknum" = 5, "winnerID" = finalist2[1], "loserID" = finalist2[2], 'matchlevel' = "Semifinal", seasonnum = seasonnum)
  ktfinal <- fight(finalist1[1], finalist2[1], df)
  result.final <- data.frame("weeknum" = 6, "winnerID" = ktfinal[1], "loserID" = ktfinal[2], 'matchlevel' = "Final", seasonnum = seasonnum)
  battle.log <- rbind(battle.log, result.frame1, result.frame2, result.final)
  return(battle.log)
}


