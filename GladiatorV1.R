library(dplyr)
library(ggplot2)

### Generating Fighters
genFighter <- function(startID=1, n=1, year=1, mean.val = 500, sd.val = 120, coe.sd = .05){
  physical.element <- rnorm(n=n, mean=mean.val*0.3, sd = sd.val*0.3)
  mental.element <- rnorm(n=n, mean=0, sd = sd.val*0.2)
  st.base <- rnorm(n=n, mean = mean.val*0.7, sd = sd.val*0.7) + physical.element
  sp.base <- rnorm(n=n, mean = mean.val*0.7, sd = sd.val*0.7) + physical.element
  wt.base <- rnorm(n=n, mean = mean.val, sd = sd.val*0.8) + mental.element
  ag.base <- rnorm(n=n, mean = mean.val, sd = sd.val) - mental.element
  barracks <- data.frame('playerID' = c(startID:(startID+n-1)), 'born' = rep(year, n), 'skill.base' = rnorm(n=n, mean=mean.val, sd = sd.val), 'strength.base' = st.base, 'speed.base' = sp.base , 'wit.base' = wt.base, 'aggression.base' = ag.base, 'skill.coe' = rnorm(n=n, mean=0, sd = coe.sd), 'speed.coe' = rnorm(n=n, mean=0, sd = coe.sd), 'strength.coe' = rnorm(n=n, mean=0, sd = coe.sd) , 'wit.coe' = rnorm(n=n, mean=0, sd = coe.sd))
  return(barracks)
}
#barracks <- data.frame('playerID' = c(1:10), 'born' = 1, 'skill.base' = rnorm(n=10, mean=500, sd = 120), 'strength.base' = rnorm(n=10, mean=500, sd = 120), 'wit.base' = rnorm(n=10, mean=500, sd = 120), 'speed.base' = rnorm(n=10, mean=500, sd = 120), 'aggression.base' = rnorm(n=10, mean=500, sd = 120))

#### Aging Curves
sp.age <- c(seq(from = 0.8, to = 1, by = 0.04), rep(1, 5), seq(from = 1, to = 0.8, by = -0.03), seq(from = 0.8, to = 0.5, by = -0.01))
st.age <- c(seq(from = 0.7, to = 1, by = 0.05), rep(1, 5), seq(from = 1, to = 0.8, by = -0.02), seq(from = 0.8, to = 0.4, by = -0.02))
wit.age <- c(seq(from = 0.5, to = 0.9, by = 0.08), seq(from = 0.9, to = 1, by = 0.02), rep(1, 2), seq(from = 1, to = 1.3, by = 0.01))
sk.age <- c(seq(from = 0.5, to = 0.9, by = 0.05), seq(from = 0.9, to = 1, by = 0.02), rep(1, 8), seq(from = 1, to = 0.8, by = -0.01))
age <- c(15:(15+43))
sp.age <- sp.age[1:44]
wit.age <- wit.age[1:44]
age.curve <- as.data.frame(cbind(age, sp.age, st.age, sk.age, wit.age))

ggplot(age.curve, aes(x=age)) + geom_line(aes(y=sp.age), col = "green") + geom_line(aes(y=st.age), col = "red") + geom_line(aes(y=sk.age), col = "orange") + geom_line(aes(y=wit.age), col = "blue")


### Aging Function
ageFighter <- function(df, newyear, ac =age.curve, coe.sd = 0.03){
  df$age <- newyear - df$born
  df$skill.coe <- df$skill.coe + rnorm(n=nrow(df), mean=0, sd = 0.05)
  df$speed.coe <- df$speed.coe + rnorm(n=nrow(df), mean=0, sd = 0.05)
  df$strength.coe <- df$strength.coe + rnorm(n=nrow(df), mean=0, sd = 0.05)
  df$wit.coe <- df$wit.coe + rnorm(n=nrow(df), mean=0, sd = 0.05)
  df.full <- left_join(df, ac, by = "age")
  df$strength <- df$strength.base * (df.full$st.age + df.full$strength.coe)
  df$speed <- df$speed.base * (df.full$sp.age + df.full$speed.coe)
  df$wit <- df$wit.base * (df.full$wit.age + df.full$wit.coe)
  df$skill <- df$skill.base * (df.full$sk.age + df.full$skill.coe)
  df$aggression <- df$aggression.base
  return(df)
}

x <- genFighter(n=20)
x <- ageFighter(newyear=19, df=x)

### Battle Function
fight <- function(home, away, df, rand.mean = 500, rand.sd = 200){
  hdf <- subset(df, playerID == home)
  adf <- subset(df, playerID == away)
  diff.scores <- hdf[,c("skill", "strength", "wit", "speed")]/adf[,c("skill", "strength", "wit", "speed")]
  rand.vals <- rnorm(4, mean= rand.mean, sd = rand.sd)
  total.scores <- as.integer(diff.scores * rand.vals)
  winner <- ifelse(mean(total.scores) > rand.mean, home, away)
  loser <- ifelse(winner == home, away, home)
  return(c("winner" = winner, "loser" = loser))
}

### Initial Tournament
youngTourney <- function(df){
  df$
}

#### Checking Victory Probabilities
all <- rep(0, 1000*9)
all <- as.matrix(all)
dim(all) <- c(1000, 9)

for(jj in 1:1000){
  for(kk in 1:9){
    all[jj, kk] <- fight(10,kk,barracks)[1]
  }
  print(jj)
}

for(mm in 1:9){
  col <- all[,mm]
  perwon <- length(col[col == 10])/length(col)
  print(paste("Beat", mm, (perwon*100), "% of the time"))
}

length(all[all == 10])


