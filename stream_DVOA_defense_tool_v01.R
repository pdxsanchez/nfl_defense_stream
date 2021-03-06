
library(tidyverse) #filter(); select(); 
library(readxl) #read_excel()
library(ggplot2)
library(lubridate) 
library(stringr)
library(XML) #webscraping
library(RCurl) #webscraping


###################
### Data Import ###
###################
#setwd(paste0("~/Code/R/"))  #mac


###########################
### DVOA Find and Munge ###
###########################

u <- "http://www.footballoutsiders.com/stats/teamoff"

xData <- getURL(u)
table = readHTMLTable(xData, stringsAsFactors=F)

df0 <- table[[1]]
#########################################
## Need this code for latest version of DVOA table...
df01 <- df0[-1,]
names(df01)[1]  <- "DVOA.RANK"
names(df01)[2]  <- "TEAM"
names(df01)[3]  <- "OFFENSEDVOA"
names(df01)[4]  <- "LAST.WEEK"
names(df01)[5]  <- "OFFENSEDAVE"
names(df01)[6]  <- "DAVE.RANK"
names(df01)[7]  <- "PASS.OFF"
names(df01)[8]  <- "PASS.RANK"
names(df01)[9]  <- "RUSH.OFF"
names(df01)[10] <- "RUSH.RANK"
#########################################
df1 <- df01 %>% filter(!is.na(TEAM)) %>%                                         #remove page break
                filter(TEAM != "TEAM")  %>%
                filter(TEAM != "TOTAL") %>%
                mutate_at(vars(DVOA.RANK,DAVE.RANK,PASS.RANK,RUSH.RANK),           # chr->nums
                          funs(as.numeric(.)))   %>%
                mutate_at(vars(`OFFENSEDVOA`,`OFFENSEDAVE`,`PASS.OFF`,`RUSH.OFF`), #Remove '%'
                          funs(as.numeric(gsub("\\%", "", .))) )                 #then chr->nums

saveRDS(df1,"FO_2017_DVOA_data.rds")
df2 <- readRDS("FO_2017_DVOA_data.rds")
lookup <- df2 %>% select (TEAM,OFFENSEDVOA) %>%
                   dplyr::rename(OPPX = TEAM) %>% #rename DVOA abbreviation to fit ESPN Sched
                   mutate(OPPX = ifelse(OPPX=="LARM","LAR",OPPX)) %>%
                   mutate(OPPX = ifelse(OPPX=="LACH","LAC",OPPX)) %>%
                   mutate(OPPX = ifelse(OPPX=="JAC" ,"JAX",OPPX)) %>%
                   mutate(OPPX = ifelse(OPPX=="WAS" ,"WSH",OPPX)) 

###########################
### ESPN Schedule Grid  ###
###########################

url <- "http://www.espn.com/nfl/schedulegrid"
x   <- getURL(url)
tbl <- readHTMLTable(x, stringsAsFactors=F)

sc0 <- tbl[[1]]
colnames(sc0) = sc0[1, ]  #Get rid of weird 'V1,V2,...' column headers
sc1 <- sc0 %>% filter(TEAM != "TEAM") %>%  # remove duplicate rows
               arrange(TEAM)

saveRDS(sc1,"ESPN_2017_Sched_Grid.rds")
sc2 <- readRDS("ESPN_2017_Sched_Grid.rds")

##########################################
### Merge Schedule and Offensive DVOA  ###
##########################################

m0  <- sc2 %>%
       gather("WEEK","OPP",2:18) %>%
       mutate_at( vars(WEEK), funs(as.numeric(.)) ) %>%
       arrange(WEEK,TEAM) %>%
       mutate(OPPX = gsub("\\@","",OPP)) %>%
       mutate(WHERE = ifelse(grepl("@",OPP),"AWAY","HOME")) %>%
      
       left_join(lookup, by = "OPPX") %>%
       mutate(OFFENSEDVOA = ifelse(is.na(OFFENSEDVOA),0,OFFENSEDVOA)) 

m1   <- m0 %>% select(TEAM,WEEK,OFFENSEDVOA) %>%
        spread(WEEK,OFFENSEDVOA)
m1.5 <- m1 %>% select(TEAM)

m2   <- m1 %>% select(7:10) %>% mutate(sum=rowSums(.))
 
m2.1 <- m2 %>% select(sum)
sc4  <- sc1 %>% select(7:10)

m3   <- cbind(m1.5,sc4)
m4   <- cbind(m3,m2.1)

Defense.DVOA <- m4 %>% arrange(sum)
View(Defense.DVOA)
 