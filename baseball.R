
library(Lahman)
library(tidyr)
library(dplyr)

# make team data frame with necessary variables
teams = Teams
teams = subset(teams, select = c(yearID, lgID, teamID, Rank, G, W, L)) 

# grab teams from 1985 and on
teams = filter(teams, yearID >=1985)
# add winning % column
teams = mutate(teams, winpercent = W/(W+L))
# add salaries to a data frame
sals = Salaries                      

# create function to seperate salaries by year
salyear <- function(x){
sals %>% filter(yearID == x)
}
y85 <- salyear(1985)
y86 <- salyear(1986)
y87 <- salyear(1987)
y88 <- salyear(1988)
y89 <- salyear(1989)
y90 <- salyear(1990)
y91 <- salyear(1991)
y92 <- salyear(1992)
y93 <- salyear(1993)
y94 <- salyear(1994)
y95 <- salyear(1995)
y96 <- salyear(1996)
y97 <- salyear(1997)
y98 <- salyear(1998)
y99 <- salyear(1999)
y00 <- salyear(2000)
y01 <- salyear(2001)
y02 <- salyear(2002)
y03 <- salyear(2003)
y04 <- salyear(2004)
y05 <- salyear(2005)
y06 <- salyear(2006)
y07 <- salyear(2007)
y08 <- salyear(2008)
y09 <- salyear(2009)
y10 <- salyear(2010)
y11 <- salyear(2011)
y12 <- salyear(2012)
y13 <- salyear(2013)
y14 <- salyear(2014)
y15 <- salyear(2015)
y16 <- salyear(2016)

# create a function to sum payroll by team
teampayroll <- function(x){
x %>% 
  group_by(teamID) %>% 
    mutate(payroll = sum(salary))
}

y85 <- teampayroll(y85)
y86 <- teampayroll(y86)
y87 <- teampayroll(y87)
y88 <- teampayroll(y88)
y89 <- teampayroll(y89)
y90 <- teampayroll(y90)
y91 <- teampayroll(y91)
y92 <- teampayroll(y92)
y93 <- teampayroll(y93)
y94 <- teampayroll(y94)
y95 <- teampayroll(y95)
y96 <- teampayroll(y96)
y97 <- teampayroll(y97)
y98 <- teampayroll(y98)
y99 <- teampayroll(y99)
y00 <- teampayroll(y00)
y01 <- teampayroll(y01)
y02 <- teampayroll(y02)
y03 <- teampayroll(y03)
y04 <- teampayroll(y04)
y05 <- teampayroll(y05)
y06 <- teampayroll(y06)
y07 <- teampayroll(y07)
y08 <- teampayroll(y08)
y09 <- teampayroll(y09)
y10 <- teampayroll(y10)
y11 <- teampayroll(y11)
y12 <- teampayroll(y12)
y13 <- teampayroll(y13)
y14 <- teampayroll(y14)
y15 <- teampayroll(y15)
y16 <- teampayroll(y16)

# function to remove all but one from team
rmdup <- function(x){
  x[!duplicated(x$teamID),]
}

y85 <- rmdup(y85)
y86 <- rmdup(y86)
y87 <- rmdup(y87)
y88 <- rmdup(y88)
y89 <- rmdup(y89)
y90 <- rmdup(y90)
y91 <- rmdup(y91)
y92 <- rmdup(y92)
y93 <- rmdup(y93)
y94 <- rmdup(y94)
y95 <- rmdup(y95)
y96 <- rmdup(y96)
y97 <- rmdup(y97)
y98 <- rmdup(y98)
y99 <- rmdup(y99)
y00 <- rmdup(y00)
y01 <- rmdup(y01)
y02 <- rmdup(y02)
y03 <- rmdup(y03)
y04 <- rmdup(y04)
y05 <- rmdup(y05)
y06 <- rmdup(y06)
y07 <- rmdup(y07)
y08 <- rmdup(y08)
y09 <- rmdup(y09)
y10 <- rmdup(y10)
y11 <- rmdup(y11)
y12 <- rmdup(y12)
y13 <- rmdup(y13)
y14 <- rmdup(y14)
y15 <- rmdup(y15)
y16 <- rmdup(y16)

sals <- rbind(y85, y86, y87, y88, y89, y90, y91, y92, y93, y94, y95, y96, y97, y98, y99, y00, y01, y02, y03, y04, y05, y06, y07, y08, y09, y10, y11, y12, y13, y14, y15, y16)
sals <- subset(sals, select = c(yearID, teamID, payroll))

sals$teamID[sals$teamID=="CHN"] <- "CHC"
sals$teamID[sals$teamID=="CHA"] <- "CHW"
sals$teamID[sals$teamID=="KCA"] <- "KCR"
sals$teamID[sals$teamID=="LAN"] <- "LAD"
sals$teamID[sals$teamID=="NYN"] <- "NYM"
sals$teamID[sals$teamID=="NYA"] <- "NYY"
sals$teamID[sals$teamID=="SDN"] <- "SDP"
sals$teamID[sals$teamID=="SFN"] <- "SFG"
sals$teamID[sals$teamID=="SLN"] <- "STL"
sals$teamID[sals$teamID=="TBA"] <- "TBR"
sals$teamID[sals$teamID=="WSN"] <- "WAS"

teams$teamID <- as.character(teams$teamID)
teams$teamID[teams$teamID=="CHN"] <- "CHC"
teams$teamID[teams$teamID=="CHN"] <- "CHC"
teams$teamID[teams$teamID=="CHA"] <- "CHW"
teams$teamID[teams$teamID=="KCA"] <- "KCR"
teams$teamID[teams$teamID=="LAN"] <- "LAD"
teams$teamID[teams$teamID=="NYN"] <- "NYM"
teams$teamID[teams$teamID=="NYA"] <- "NYY"
teams$teamID[teams$teamID=="SDN"] <- "SDP"
teams$teamID[teams$teamID=="SFN"] <- "SFG"
teams$teamID[teams$teamID=="SLN"] <- "STL"
teams$teamID[teams$teamID=="TBA"] <- "TBR"
teams$teamID[teams$teamID=="WSN"] <- "WAS"
teams$teamID <- as.factor(teams$teamID)

baseball <- merge(teams, sals, by=c("yearID", "teamID"))
baseball$teamID[baseball$teamID=="ML4"] <- "MIL"
plot(baseball$payroll, baseball$winpercent, col=baseball$yearID)
baseball <- baseball %>% mutate(dolperwin = payroll/W)
plot(baseball$yearID,baseball$dolperwin, col=baseball$teamID)






