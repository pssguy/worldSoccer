library(engsoccerdata)
library(ggvis)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyr)
library(markdown)
library(readr)
library(doBy)

library(rcdimple)
library(plotly)
library(crosstalk)
#df <- tbl_df(engsoccerdata2)
#df$gameDate <- as.Date(df$Date) # takes a while

df <- readRDS("updated.rds")


## make tables - shpuld look at make table function


df_home <- df %>% select(team=home, opp=visitor, GF=hgoal, GA=vgoal,Season,tier,division,Date) %>% mutate(venue="home")
df_away <- df %>% select(team=visitor, opp=home, GF=vgoal, GA=hgoal,Season,tier,division,Date) %>% mutate(venue="away")

temp <- rbind(df_home,df_away)
# temp <-
#   rbind(
#     df %>% select(team=home, opp=visitor, GF=hgoal, GA=vgoal,Season,tier,division,Date),
#     df %>% select(team=visitor, opp=home, GF=vgoal, GA=hgoal,Season,tier,division,Date)
#   )

temp <- temp %>% 
  mutate(Date=as.Date(Date)) %>% 
  arrange(Date)

## do adjust based on points for win

new<-
  temp %>%
  filter(Season>=1981) %>% 
  mutate(GD = GF-GA) %>%
  group_by(team,Season,tier,division) %>%
  summarize(GP = n(),
            gf = sum(GF),
            ga = sum(GA),
            gd = sum(GD),
            W = sum(GD>0),
            D = sum(GD==0),
            L = sum(GD<0)
  ) %>%
  mutate(Pts = (W*3) + D) %>%
  arrange(desc(Pts),desc(gd),desc(gf),team)

old<-
  temp %>%
  filter(Season<=1980) %>% 
  mutate(GD = GF-GA) %>%
  group_by(team,Season,tier,division) %>%
  summarize(GP = n(),
            gf = sum(GF),
            ga = sum(GA),
            gd = sum(GD),
            W = sum(GD>0),
            D = sum(GD==0),
            L = sum(GD<0)
  ) %>%
  mutate(Pts = (W*2) + D) %>%
  arrange(desc(Pts),desc(gd),desc(gf),team)


## add positions both within division and overall
## may want to diff for different plots

all<- rbind(old,new) %>% 
  ungroup() %>% 
  group_by(Season,tier,division) %>% 
  arrange(desc(Pts),desc(gd),desc(gf),team) %>% 
  mutate(Position=row_number()) %>% 
  ungroup() %>% 
  arrange(tier,Position) %>% 
  group_by(Season) %>% 
  mutate(Overall=row_number()) %>% 
  ungroup()

# set to character to enbale points to be discrete colors
all$tier <- as.character(all$tier)

# set to more meaningful  South and North
all[all$division=="3a",]$division <- "3N"
all[all$division=="3b",]$division <- "3S"

#write_csv(all,"data/all.csv") ## annual experiment trelliscope finishing positions by year



teamOptions <- sort(unique(all$team))

seasonOptions <- c(1880:2015)

# get div available each year
seasonDiv <- df %>% 
  select(Season,division) %>% 
  unique() %>% 
  arrange(desc(Season))

#print(seasonDiv)

## PFA 
pfa <- read_csv("pfa.csv")

teamChoice <- sort(unique(pfa$team))
playerChoice <- sort(unique(pfa$player))
countryChoice <- sort(unique(pfa$country))


## Deloitte

allYears <- read_csv("bigClubsDeloitte.csv")

## Update annually
topTen <-allYears %>% 
  filter(Year==2013) %>% 
  head(.,10) %>% 
  .$Club

print("done global")