
library(Lahman)
library(tidyr)
library(dplyr)
library(ggplot2)

# make team data frame with necessary variables
teams = Teams
teams = subset(teams, select = c(yearID, lgID, teamID, Rank, G, W, L)) 

# grab teams from 1985 and on to match with years of payroll information
teams = filter(teams, yearID >=1985)

# add winning % column
teams = mutate(teams, winpercent = round((W/(W+L)), digits = 3))

# add salaries to a data frame
sals = Salaries          

# Get team payroll for each year
sals <- sals %>% 
  group_by(yearID, teamID) %>% 
    summarise(payroll = sum(salary))

#Standardize team names 
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

# merge data into baseball
baseball <- merge(teams, sals, by=c("yearID", "teamID"))

baseball$teamID[baseball$teamID=="ML4"] <- "MIL"
baseball$yearID <- as.factor(baseball$yearID)

# calculate Amount of payroll per team win
baseball <- baseball %>% mutate(dolperwin = payroll/W)

#calculate total payroll per year for entire MLB
mlb <- baseball %>% 
  group_by(yearID) %>% 
     summarise(mlbpayroll = sum(as.numeric(payroll)))

# get number of teams for each year
 numteams <-  baseball %>% 
  group_by(yearID) %>%
  summarise(numteams =length(yearID))

mlb$numteams <- numteams$numteams

# calculate average payroll per year
mlb <- mlb %>% 
  mutate(avgpayroll = mlbpayroll/numteams)
mlb <- subset(mlb, select = -c(numteams))

# merge into baseball data set
baseball <- merge(baseball, mlb, by="yearID")

# calculate percentange of total payroll each team has
baseball <- baseball %>% 
  mutate(percentofmlb = round((payroll/mlbpayroll), digits = 4))

#calculate average dollars of mlb payroll per win
wins <- baseball %>% 
  group_by(yearID) %>% 
  summarise(mlbwin= (sum(W)))

baseball <- merge(baseball, wins, by="yearID")
baseball <- baseball %>% 
  mutate(mlbdolperwin = mlbpayroll/mlbwin)

baseball <- baseball %>% group_by(yearID) %>% mutate(payrank = dense_rank((desc(payroll))))



ggplot(subset(baseball, teamID =="KCR"), aes(payrank, winpercent)) + geom_point(color="blue") + labs(title = "Royals", x = "Payroll Rank") 



lm <- lm(baseball$winpercent~baseball$payrank)
summary(lm)