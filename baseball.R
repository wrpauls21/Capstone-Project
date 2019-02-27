########## Capstone Project ################

# Load Packages
library(DT)
library(readr)
library(Lahman)
library(tidyr)
library(dplyr)
library(car)
library(MASS)
library(gvlma)
library(ggplot2)

############ Data Wrangling ########################

# make team data frame with necessary variables
teams = Teams
teams = subset(teams, select = c(yearID, lgID, teamID, Rank, G, W, L, DivWin, WCWin)) 

# grab teams from 1985 and on to match with years of payroll information
teams = filter(teams, yearID >=1985)

# add winning % column
teams = mutate(teams, winpercent = round((W/(W+L)), digits = 3))

# add a playoffs column
teams$DivWin[is.na(teams$DivWin)] <- "N"
teams$WCWin[is.na(teams$WCWin)] <- "N"
teams <- mutate(teams, playoffs = if_else(DivWin == 'Y' | WCWin == 'Y', 'Y', 'N'))
teams = subset(teams, select = -c(DivWin, WCWin))
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
baseball$playoffs <- as.factor(baseball$playoffs)

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

# calculate teams difference in payroll from previous year
baseball<- baseball %>%
  arrange(teamID, yearID) %>%
  group_by(teamID) %>%
  mutate(paydiff = payroll - lag(payroll)) 

# +/- % change of payroll from previous year
baseball<- baseball %>%
  arrange(teamID, yearID) %>%
  group_by(teamID) %>%
  mutate(percentdiff = paydiff/lag(payroll)) 

baseball$percentdiff <- round(baseball$percentdiff, 4)

## calculate change in winning percentage from previous year
baseball<- baseball %>%
  arrange(teamID, yearID) %>%
  group_by(teamID) %>%
  mutate(winpercentdiff = winpercent - lag(winpercent)) 

## calculate change in percent of mlb from previous year
baseball<- baseball %>%
  arrange(teamID, yearID) %>%
  group_by(teamID) %>%
  mutate(percentofmlbdiff = percentofmlb - lag(percentofmlb)) 

## calculate change in payroll rank from previous year
baseball<- baseball %>%
  arrange(teamID, yearID) %>%
  group_by(teamID) %>%
  mutate(payrankdiff = payrank - lag(payrank)) 

# create column to test if payroll increased for team
baseball$payincreased[baseball$paydiff<=0]<- "No"
baseball$payincreased[baseball$paydiff>0]<- "Yes"

baseball$payincreased <- as.factor(baseball$payincreased)

# create a column to test if winning % increased for team
baseball$winincreased[baseball$winpercentdiff<=0]<- "No"
baseball$winincreased[baseball$winpercentdiff>0]<- "Yes"

baseball$winincreased <- as.factor(baseball$winincreased)

# create a column to test if percent of mlb increased for team
baseball$percentofmlbincreased[baseball$percentofmlbdiff<=0]<- "No"
baseball$percentofmlbincreased[baseball$percentofmlbdiff>0]<- "Yes"

baseball$percentofmlbincreased <- as.factor(baseball$percentofmlbincreased)

# create column to test if payroll increased for team
baseball$payrankincreased[baseball$payrankdiff<=0]<- "No"
baseball$payrankincreased[baseball$payrankdiff>0]<- "Yes"

baseball$payrankincreased <- as.factor(baseball$payrankincreased)

# write.csv(baseball, file = "baseball.csv")


################## Probability & Statistics #############################

# summary of teams whose winning increased after payroll rank increased
summary(baseball$winincreased[baseball$payrankincreased=="Yes"])

# summary of teams whose winning increased after payroll rank decreased
summary(baseball$winincreased[baseball$payrankincreased=="No"])


# bar plot of win % increase and Payroll Rank Increase
barplot(matrix(c(189,183,248,264),nr=2), beside=T, 
        col=c("blue","red"), ylim = c(0,300), 
        names.arg=c("Payroll Rank Increased", "Payroll Rank Decreased"))
legend("topleft", c("Win % Increased","Win % Decreased"), pch=15, 
       col=c("blue","red"), 
       bty="n")

## Z-test
payranktest <- prop.test(x = c(189, 248), n = c((189+183), (248+264)), alternative = "greater")
# Printing the results
payranktest 


# summary of teams whose winning increased after % of mlb pay increased
summary(baseball$winincreased[baseball$percentofmlbincreased=="Yes"])

# summary of teams whose winning increased after % of mlb pay decreased
summary(baseball$winincreased[baseball$percentofmlbincreased=="No"])


# bar plot of win % increase and % of MLB Payroll Increase
barplot(matrix(c(224,248,213,199),nr=2), beside=T, 
        col=c("blue","red"), ylim = c(0,300),
        names.arg=c("% of MLB Payroll Increased", "% of MLB Payroll Decreased"))
legend("topleft", c("Win % Increased","Win % Decreased"), pch=15, 
       col=c("blue","red"), 
       bty="n")


# Z-Test
payrolltest <- prop.test(x = c(224, 213), n = c((224+248), (213+199)))
# Printing the results
payrolltest

################ Machine Learning ######################### 

#Linear regression summary with confidence interval      
MLB <- lm(baseball$winpercent~baseball$payrank)
summary(MLB)

confint(MLB)

# plot Win % vs Payrank
winvsrank <- ggplot(baseball, aes(payrank, winpercent)) + geom_jitter() +  stat_smooth(method = "lm", col = "red") + labs(title = "Win % vs Payroll Rank", x = "Payroll Rank", y = "win %") 
winvsrank + scale_x_continuous(breaks=c(5,10,15,20,25,30))


# Testing Linear Regression Assumptions


# Assessing Outliers

outlierTest(MLB) # Bonferonni p-value for most extreme obs

# Normality of Residuals

qqPlot(MLB, main="QQ Plot") #qq plot for studentized resid 

# distribution of studentized residuals

sresid <- studres(MLB) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# plot studentized residuals vs. fitted values 
plot(fitted(MLB), residuals(MLB))
abline(h = 0)
plot(fitted(MLB), abs(residuals(MLB)))
summary(lm(abs(residuals(MLB))~fitted(MLB)))


# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(MLB)


#Global validation of linear model assumptions
gvmodel <- gvlma(MLB) 
summary(gvmodel)
#########################################################

library(scales)
############## More data Visualization ######################
winvsrank <- ggplot(baseball, aes(payrank, winpercent)) + 
              geom_jitter() +  
              stat_smooth(method = "lm", col = "red") + 
              labs(title = "Win % vs Payroll Rank", x = "Payroll Rank", y = "win %") + 
              scale_x_continuous(breaks=c(5,10,15,20,25,30))
winvsrank

Royals <- subset(baseball, teamID=="KCR")
KC <- lm(winpercent~payrank, data = Royals)

KCplot <- ggplot(Royals, aes(payrank, winpercent)) + 
          geom_point() + stat_smooth(method = "lm", col = "red") + 
          labs(title = "Royals Win % vs Payroll Rank", x = "Payroll Rank", y = "win %") +
          scale_x_continuous(breaks=c(5,10,15,20,25,30))
KCplot

# same plot using whether they made playoffs as icon label
KCplot2 <- ggplot(Royals, aes(payrank, winpercent, label = playoffs, color = factor(playoffs))) +
            geom_text() + 
            stat_smooth(method = "lm", col = "red") +
            labs(title = "Royals Win % vs Payroll Rank", x = "Payroll Rank", y = "win %") +
            scale_color_manual(values = c("black" , "royalblue1"), name = "Made Playoffs?") +
            guides(size=guide_legend(title="Payroll $")) + 
            theme(axis.title = element_text(colour="black", size=16), plot.title = element_text(colour="royalblue1", size=20)) +
            scale_x_continuous(breaks=c(5,10,15,20,25,30))
KCplot2

# same plot adding payroll as icon label size
KCplot2b <- ggplot(Royals, aes(payrank, winpercent, label = playoffs, color = factor(playoffs))) +
  geom_text(aes(size= Royals$payroll)) + 
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Royals Win % vs Payroll Rank", x = "Payroll Rank", y = "win %") +
  scale_color_manual(values = c("black" , "royalblue1"), name = "Made Playoffs?") +
  guides(size=guide_legend(title="Payroll $")) + 
  theme(axis.title = element_text(colour="black", size=16), plot.title = element_text(colour="royalblue1", size=20)) +
  scale_x_continuous(breaks=c(5,10,15,20,25,30))
KCplot2b

# Royals Payroll Amount vs Rank
KCpay <- ggplot(Royals, aes(payrank, payroll)) + 
        geom_point(color = "royalblue1") + 
        labs(title = "Royals Payroll Amount vs Rank", x = "Payroll Rank", y = "Payroll") + 
        scale_y_continuous(breaks=c(25000000, 50000000,75000000, 100000000,125000000 ), labels = dollar) + 
        theme(axis.title = element_text(colour="black", size=16), plot.title = element_text(colour="black", size=20)) 
KCpay


# Various Boxplots
ggplot(baseball, aes(x = playoffs, y = payrank)) +
  geom_boxplot() +  labs(x = "Made Playoffs?", y = "Payroll Rank") 

ggplot(subset(baseball, !is.na(payincreased)), aes(x = payincreased , y = winpercent)) +
  geom_boxplot() +  labs(x = "Payroll Increased from Previous Yerar?", y = "Win %")






























