## weekly updates 
## based on https://gist.github.com/jalapic/54110279217f71ae4348
## There is a weeklyupdate backups which has more working data
## Need to revisit every year

library(XML)
library(dplyr)
library(tidyr)
library(stringr)

#scraping function
getresults2 <- function(z1){
  z1<-as.matrix(z1)
  z1<-rbind(colnames(z1), z1)
  z1[z1==""]  <- NA 
  z1<-as.data.frame(z1)
  z1<-z1[c(1:3,5,6)]
  colnames(z1)<-c("date", "time", "home","visitor", "FT")
  z1$date <- as.character(z1$date)
  z1$date <- zoo::na.locf(z1$date)
  z1$FT <- gsub(" .*$", "", z1$FT)
  z1$FT <- gsub(":", "-", as.character(z1$FT)) 
  return(z1)
}


##### Tier 1

#get urls
pages <- 1:38 #pages <- 1
myurls <- paste("http://www.worldfootball.net/schedule/eng-premier-league-2015-2016-spieltag/",
                pages, "/", sep="")

results <- vector("list",length(myurls))

for(i in 1:length(myurls)){
  x <- readHTMLTable(myurls[i])
  results[[i]] <- getresults2(x[[2]])
}

df.1 <- do.call("rbind", results)


## during season, need to remove those yet to be played

df.1 <- df.1[df.1$FT!="---",]



## Tidy data to be in engsoccerdata format


#Dates
df.1$Date <- as.Date(df.1$date, format="%d/%m/%Y")

#Fix teamnames
sort(as.character(unique(df.1$home))) #check team names
df.1$home <- gsub(" FC", "", df.1$home)
df.1$home <- gsub(" AFC", "", df.1$home)
df.1$visitor <- gsub(" FC", "", df.1$visitor)
df.1$visitor <- gsub(" AFC", "", df.1$visitor)

#add variables
df.1<-
  df.1 %>% 
  separate(FT, c("hgoal", "vgoal"), remove=F) %>%
  mutate(Season=2015,
         division=1, 
         tier=1, 
         totgoal=as.numeric(as.character(hgoal))+as.numeric(as.character(vgoal)),
         goaldif=as.numeric(as.character(hgoal))-as.numeric(as.character(vgoal)),
         result=ifelse(goaldif>0, "H", ifelse(goaldif<0, "A", "D"))
  )

df.1 <- df.1 %>% select(Date, Season, home,visitor,FT,hgoal,vgoal,division,tier,totgoal,goaldif,result)

#make sure variables are numeric
df.1$hgoal <- as.numeric(as.character(df.1$hgoal))
df.1$vgoal <- as.numeric(as.character(df.1$vgoal))

head(df.1)
str(df.1)



##### Tier 2.


#get urls
pages<-1:46
myurls <- paste("http://www.worldfootball.net/schedule/eng-championship-2015-2016-spieltag/",
                pages, "/", sep="")

results <- vector("list",length(myurls))

for(i in 1:length(myurls)){
  x <- readHTMLTable(myurls[i])
  results[[i]] <- getresults2(x[[2]])
}

df.2 <- do.call("rbind", results)

## during season, need to remove those yet to be played

df.2 <- df.2[df.2$FT!="---",]


## Tidy data to be in engsoccerdata format


#Dates
df.2$Date <- as.Date(df.2$date, format="%d/%m/%Y")

#Fix teamnames
sort(as.character(unique(df.2$home))) #check team names
df.2$home <- gsub(" FC", "", df.2$home)
df.2$visitor <- gsub(" FC", "", df.2$visitor)

#add variables
df.2<-
  df.2 %>% 
  separate(FT, c("hgoal", "vgoal"), remove=F) %>%
  mutate(Season=2015,
         division=2, 
         tier=2, 
         totgoal=as.numeric(as.character(hgoal))+as.numeric(as.character(vgoal)),
         goaldif=as.numeric(as.character(hgoal))-as.numeric(as.character(vgoal)),
         result=ifelse(goaldif>0, "H", ifelse(goaldif<0, "A", "D"))
  )

df.2 <- df.2 %>% select(Date, Season, home,visitor,FT,hgoal,vgoal,division,tier,totgoal,goaldif,result)

#make sure variables are numeric
df.2$hgoal <- as.numeric(as.character(df.2$hgoal))
df.2$vgoal <- as.numeric(as.character(df.2$vgoal))

head(df.2)
str(df.2)



##### Tier 3

#get urls
pages<-1:46
myurls <- paste("http://www.worldfootball.net/schedule/eng-league-one-2015-2016-spieltag/",
                pages, "/", sep="")

results <- vector("list",length(myurls))

for(i in 1:length(myurls)){
  x <- readHTMLTable(myurls[i])
  results[[i]] <- getresults2(x[[2]])
}

df.3 <- do.call("rbind", results)

## during season, need to remove those yet to be played

df.3 <- df.3[df.3$FT!="---",]

## Tidy data to be in engsoccerdata format


#Dates
df.3$Date <- as.Date(df.3$date, format="%d/%m/%Y")

#Fix teamnames
sort(as.character(unique(df.3$home))) #check team names
df.3$home <- gsub(" FC", "", df.3$home)
df.3$visitor <- gsub(" FC", "", df.3$visitor)
df.3$home <- gsub(" AFC", "", df.3$home)
df.3$visitor <- gsub(" AFC", "", df.3$visitor)
df.3$home <- gsub("Yeovil Town", "Yeovil", df.3$home)
df.3$visitor <- gsub("Yeovil Town", "Yeovil", df.3$visitor)


#add variables
df.3<-
  df.3 %>% 
  separate(FT, c("hgoal", "vgoal"), remove=F) %>%
  mutate(Season=2015,
         division=3, 
         tier=3, 
         totgoal=as.numeric(as.character(hgoal))+as.numeric(as.character(vgoal)),
         goaldif=as.numeric(as.character(hgoal))-as.numeric(as.character(vgoal)),
         result=ifelse(goaldif>0, "H", ifelse(goaldif<0, "A", "D"))
  )

df.3 <- df.3 %>% select(Date, Season, home,visitor,FT,hgoal,vgoal,division,tier,totgoal,goaldif,result)

#make sure variables are numeric
df.3$hgoal <- as.numeric(as.character(df.3$hgoal))
df.3$vgoal <- as.numeric(as.character(df.3$vgoal))

head(df.3)
str(df.3)


##### Tier 4

#get urls
pages<-1:46
myurls <- paste("http://www.worldfootball.net/schedule/eng-league-two-2015-2016-spieltag/",
                pages, "/", sep="")

results <- vector("list",length(myurls))

for(i in 1:length(myurls)){
  x <- readHTMLTable(myurls[i])
  results[[i]] <- getresults2(x[[2]])
}

df.4 <- do.call("rbind", results)

## during season, need to remove those yet to be played

df.4 <- df.4[df.4$FT!="---",]


## Tidy data to be in engsoccerdata format


#Dates
df.4$Date <- as.Date(df.4$date, format="%d/%m/%Y")

#Fix teamnames
sort(as.character(unique(df.4$home))) #check team names
df.4$home <- gsub(" FC", "", df.4$home)
df.4$visitor <- gsub(" FC", "", df.4$visitor)
df.4$home <- gsub(" AFC", "", df.4$home)
df.4$visitor <- gsub(" AFC", "", df.4$visitor)

df.4$home <- gsub(" Stanley", "", df.4$home)
df.4$visitor <- gsub(" Stanley", "", df.4$visitor)
df.4$home <- gsub("&", "and", df.4$home)
df.4$visitor <- gsub("&", "and", df.4$visitor)
df.4$home <- gsub("Cheltenham Town", "Cheltenham", df.4$home)
df.4$visitor <- gsub("Cheltenham Town", "Cheltenham", df.4$visitor)
df.4$home <- gsub("Stevenage", "Stevenage Borough", df.4$home)
df.4$visitor <- gsub("Stevenage", "Stevenage Borough", df.4$visitor)

#add variables
df.4<-
  df.4 %>% 
  separate(FT, c("hgoal", "vgoal"), remove=F) %>%
  mutate(Season=2015,
         division=4, 
         tier=4, 
         totgoal=as.numeric(as.character(hgoal))+as.numeric(as.character(vgoal)),
         goaldif=as.numeric(as.character(hgoal))-as.numeric(as.character(vgoal)),
         result=ifelse(goaldif>0, "H", ifelse(goaldif<0, "A", "D"))
  )

df.4 <- df.4 %>% select(Date, Season, home,visitor,FT,hgoal,vgoal,division,tier,totgoal,goaldif,result)

#make sure variables are numeric
df.4$hgoal <- as.numeric(as.character(df.4$hgoal))
df.4$vgoal <- as.numeric(as.character(df.4$vgoal))

head(df.4)
str(df.4)


### Error Checking - check against league tables using engsoccerdata's maketable() 

library(engsoccerdata)
maketable(df.1)
maketable(df.2)
maketable(df.3)
maketable(df.4)

newdf <- bind_rows(df.1,df.2,df.3,df.4)
#str(newdf)
#  division'/Date in engsoccerdata
newdf$division <- as.character(newdf$division)
newdf$Date <- as.character(newdf$Date)

## this is what comes with package so want to use/combine and then write update and create new date fiels gameDate

df <- tbl_df(engsoccerdata2)
#str(df)

#df$gameDate <- as.Date(df$Date)

updated.df <- bind_rows(df,newdf) 
updated.df$gameDate <- as.Date(updated.df$Date)


saveRDS(updated.df,"updated.rds")



### look at other leagues eg bundesliga


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
library(XML)
library(stringr)



## weekly update


#Germany
pages <- 1:34 # bundesliga has 34 rounds
myurls <- paste("http://www.worldfootball.net/schedule/bundesliga-2015-2016-spieltag/",
                pages, "/", sep="")

results <- vector("list",length(myurls))

for(i in 1:length(myurls)){
  print(i)
  x <- readHTMLTable(myurls[i])
  results[[i]] <- getresults2(x[[2]])
}

df.1 <- do.call("rbind", results)


## during season, need to remove those yet to be played

df.1 <- df.1[df.1$FT!="---",] #306 none removed in 14/15 which makes sense




#add variables but not division
df.1<-
  df.1 %>% 
  separate(FT, c("hgoal", "vgoal"), remove=F) %>%
  mutate(Season=2015,
         
         tier=1
 
  )

#df.1 <- df.1 %>% select(Date, Season, home,visitor,FT,hgoal,vgoal,tier,totgoal,goaldif,result)
df.1 <- df.1 %>% select(Date, Season, home,visitor,FT,hgoal,vgoal,tier)

#make sure variables are numeric
df.1$hgoal <- as.integer(as.character(df.1$hgoal))
df.1$vgoal <- as.integer(as.character(df.1$vgoal))
df.1$time <- NULL
df.1$Season <- as.integer(df.1$Season)
df.1$home <-as.character(df.1$home)
df.1$visitor <-as.character(df.1$visitor)
df.1$tier <- as.integer(df.1$tier)

day <- str_sub(df.1$date,1,2)
month <- str_sub(df.1$date,4,5)
year <- str_sub(df.1$date,7,10)

df.1$date <- paste(year,month,day,sep="-")




bundesliga <- readRDS("bundesliga_14.rds")
updated.df <- bind_rows(bundesliga,df.1) 


saveRDS(updated.df,"bundesliga.rds")




# Italy weekly

pages <- 1:38 
myurls <- paste("http://www.worldfootball.net/schedule/ita-serie-a-2015-2016-spieltag/",
                pages, "/", sep="")

results <- vector("list",length(myurls))

for(i in 1:length(myurls)){
  print(i)
  x <- readHTMLTable(myurls[i])
  results[[i]] <- getresults2(x[[2]])
}

df.1 <- do.call("rbind", results)


## during season, need to remove those yet to be played

df.1 <- df.1[df.1$FT!="---",] #306 none removed in 14/15 which makes sense




## Tidy data to be in engsoccerdata format str(df.1)



#add variables but not division
df.1<-
  df.1 %>% 
  separate(FT, c("hgoal", "vgoal"), remove=F) %>%
  mutate(Season=2015,
         
         tier=1
         
  )


df.1 <- df.1 %>% select(Date, Season, home,visitor,FT,hgoal,vgoal,tier)

#make sure variables are numeric
df.1$hgoal <- as.integer(as.character(df.1$hgoal))
df.1$vgoal <- as.integer(as.character(df.1$vgoal))
df.1$time <- NULL
df.1$Season <- as.integer(df.1$Season)
df.1$home <-as.character(df.1$home)
df.1$visitor <-as.character(df.1$visitor)
df.1$tier <- as.integer(df.1$tier)

day <- str_sub(df.1$date,1,2)
month <- str_sub(df.1$date,4,5)
year <- str_sub(df.1$date,7,10)

df.1$date <- paste(year,month,day,sep="-")

# head(df.1)
# str(df.1)
# 
# glimpse(df_13)
# glimpse(df.1)

calcio <- readRDS("calcio_14.rds")

updated.df <- bind_rows(calcio,df.1) 
#updated.df$gameDate <- as.Date(updated.df$Date)

saveRDS(updated.df,"calcio.rds")

calcio <- readRDS("calcio.rds")
glimpse(calcio)



# Spain weekly

pages <- 1:38 
myurls <- paste("http://www.worldfootball.net/schedule/esp-primera-division-2015-2016-spieltag/",
                pages, "/", sep="")

results <- vector("list",length(myurls))

for(i in 1:length(myurls)){
  print(i)
  x <- readHTMLTable(myurls[i])
  results[[i]] <- getresults2(x[[2]])
}

df.1 <- do.call("rbind", results)


## during season, need to remove those yet to be played

df.1 <- df.1[df.1$FT!="---",] #306 none removed in 14/15 which makes sense




## Tidy data to be in engsoccerdata format str(df.1)



#add variables but not division
df.1<-
  df.1 %>% 
  separate(FT, c("hgoal", "vgoal"), remove=F) %>%
  mutate(Season=2015,
         
         tier=1
         
  )


df.1 <- df.1 %>% select(Date, Season, home,visitor,FT,hgoal,vgoal,tier)

#make sure variables are numeric
df.1$hgoal <- as.integer(as.character(df.1$hgoal))
df.1$vgoal <- as.integer(as.character(df.1$vgoal))
df.1$time <- NULL
df.1$Season <- as.integer(df.1$Season)
df.1$home <-as.character(df.1$home)
df.1$visitor <-as.character(df.1$visitor)
df.1$tier <- as.integer(df.1$tier)

day <- str_sub(df.1$date,1,2)
month <- str_sub(df.1$date,4,5)
year <- str_sub(df.1$date,7,10)

df.1$date <- paste(year,month,day,sep="-")

# head(df.1)
# str(df.1)
# 
# glimpse(df_13)
# glimpse(df.1)

laliga <- readRDS("laliga_14.rds")

updated.df <- bind_rows(laliga,df.1) 
#updated.df$gameDate <- as.Date(updated.df$Date)

saveRDS(updated.df,"laliga.rds")

laliga <- readRDS("laliga.rds")
glimpse(laliga)




# Holland weekly

pages <- 1:34 
myurls <- paste("http://www.worldfootball.net/schedule/ned-eredivisie-2015-2016-spieltag/",
                pages, "/", sep="")

results <- vector("list",length(myurls))

for(i in 1:length(myurls)){
  print(i)
  x <- readHTMLTable(myurls[i])
  results[[i]] <- getresults2(x[[2]])
}

df.1 <- do.call("rbind", results)


## during season, need to remove those yet to be played

df.1 <- df.1[df.1$FT!="---",] #306 none removed in 14/15 which makes sense




## Tidy data to be in engsoccerdata format str(df.1)



#add variables but not division
df.1<-
  df.1 %>% 
  separate(FT, c("hgoal", "vgoal"), remove=F) %>%
  mutate(Season=2015,
         
         tier=1
         
  )


df.1 <- df.1 %>% select(Date, Season, home,visitor,FT,hgoal,vgoal,tier)

#make sure variables are numeric
df.1$hgoal <- as.integer(as.character(df.1$hgoal))
df.1$vgoal <- as.integer(as.character(df.1$vgoal))
df.1$time <- NULL
df.1$Season <- as.integer(df.1$Season)
df.1$home <-as.character(df.1$home)
df.1$visitor <-as.character(df.1$visitor)
df.1$tier <- as.integer(df.1$tier)

day <- str_sub(df.1$date,1,2)
month <- str_sub(df.1$date,4,5)
year <- str_sub(df.1$date,7,10)

df.1$date <- paste(year,month,day,sep="-")

# head(df.1)
# str(df.1)
# 
# glimpse(df_13)
# glimpse(df.1)

eredivise <- readRDS("eredivise_14.rds")

updated.df <- bind_rows(eredivise,df.1) 
#updated.df$gameDate <- as.Date(updated.df$Date)

saveRDS(updated.df,"eredivise.rds")

eredivise <- readRDS("eredivise.rds")
glimpse(eredivise)



