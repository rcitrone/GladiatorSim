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



### Studying Player Generation
years <- c(17:60)
dirty100 <- genFighter(n=1000)
dirty100 <- ageFighter(dirty100, newyear=16)
dcareer <- dirty100
for(jj in 1:length(years)) {
  yr <- years[jj]
  dirty100 <- ageFighter(dirty100, newyear=yr)
  dcareer <- rbind(dcareer, dirty100)
}


dcareer$playerIDf <- as.factor(dcareer$playerID)

library(ggthemes)
dcareer$tech <- dcareer$skill + dcareer$wit
dcareer$phys <- dcareer$strength + dcareer$speed
dcareer$potential <- dcareer$skill.base + dcareer$strength.base + dcareer$speed.base + dcareer$wit.base
dcareer$total.ability <- dcareer$skill + dcareer$strength + dcareer$wit + dcareer$speed
dcareer$age.group <- as.factor(substr(rand100$age, 1,1))
dcareer$rand <- runif(n=nrow(dcareer), min=0, max=1)
dcareer <- dcareer[order(dcareer$rand),]
rand100 <- dcareer[!duplicated(dcareer$playerID),]

ggplot(rand100, aes(x=tech, y=phys)) + geom_point(aes(colour = age, size = potential)) + theme_hc() + xlab("Technical Ability") + ylab("Physical Ability") + labs(title = "Gladiatior Ability by Age", subtitle = "Gladiator Simulation Project", caption = "@Pyrollamas")

user99 <- subset(dcareer, playerID < 99 & playerID > 90)
ggplot(user99, aes(x=age, y=speed, colour = speed.coe, by = playerIDf)) + geom_line() + geom_point(size = 2) + theme_hc() + scale_colour_gradient(low = "red", high = "green")
ggplot(user99, aes(x=age, y=skill, colour = skill.coe, by = playerIDf)) + geom_line() + geom_point(size = 2) + theme_hc() + scale_colour_gradient(low = "red", high = "green")
ggplot(user99, aes(x=age, y=wit, colour = wit.coe, by = playerIDf)) + geom_line() + geom_point(size = 2) + theme_hc() + scale_colour_gradient(low = "red", high = "green")
ggplot(user99, aes(x=age, y=strength, colour = strength.coe, by = playerIDf)) + geom_line() + geom_point(size = 2) + theme_hc() + scale_colour_gradient(low = "red", high = "green")
# 
# ts <- 6
# spiral <- subset(dcareer, playerID < 100)
# ggplot(spiral, aes(x=age))  + theme_hc() + xlab("Age") + ylab("Ability") + labs(title = "Gladiatior Ability by Age", subtitle = "Gladiator Simulation Project", caption = "@Pyrollamas") + geom_smooth(aes(y=skill), method = "loess", se= FALSE, col = "red", size = 2) +  geom_smooth(aes(y=speed), method = "loess", se= FALSE, col = "blue", size = 2)  +  geom_smooth(aes(y=wit), method = "loess", se= FALSE, col = "grey", size = 2) +
#   geom_smooth(aes(y=strength), method = "loess", se= FALSE, col = "gold", size = 2) + geom_text(aes(x=50, y=540), label = "Wit", col = "grey", size = ts) + geom_text(aes(x=50, y=420), label = "Skill", col = "red", size = ts) + geom_text(aes(x=50, y=290), label = "Strength", col = "gold", size = ts) + geom_text(aes(x=50, y=220), label = "Speed", col = "Blue", size = ts)

