library(tidyverse)
library(reshape)
library(stringr)
library(dplyr)
require(dplyr)

#LifeExpectancy
life <- read.csv("LifeExpectancy.csv")
head(life)
life <- life[, c(1,5:65)]
head(life)

#Reshaping the dataset
life <- melt(life, id=c("Country.Name"))
head(life)

#Renaming the value column to Life Expectancy
names(life)[names(life) == 'value'] <- 'Life.Expectancy'
names(life)[names(life) == 'variable'] <- 'Year'

life2 <- life[order(life$Country.Name),]
head(life2)

#----------------------------------------
#InfantMortality
infant <- read.csv("InfantMortality.csv")
infant <- infant[, c(1,5:65)]
head(infant)

#Reshaping the dataset
infant <- melt(infant, id=c("Country.Name"))
head(infant)

life$Infant.Mortality <- infant$value
head(life)

#----------------------------------------
#Population
pop <- read.csv("Population.csv")
pop <- pop[, c(1,5:65)]
head(pop)

#Reshaping the dataset
pop <- melt(pop, id=c("Country.Name"))
head(pop)

life$Population <- pop$value 
head(life)

#----------------------------------------
#GDP
gdp <- read.csv("GDPper_capita.csv")
gdp <- gdp[, c(1,5:65)]
head(gdp)

#Reshaping the dataset
gdp <- melt(gdp, id=c("Country.Name"))
head(gdp)

life$GDP.Per.Capita <- gdp$value
head(life)

#----------------------------------------
#CauseOfDeath
cause <- read.csv("CauseOfDeath.csv")

life$PrimaryKey <- str_c(life$Country.Name ,life$Year)
cause$PrimaryKey <- str_c(cause$Entity,'X',cause$Year)

data <- merge(life, cause, by="PrimaryKey")
head(data)

#data <- data[-c('PrimaryKey','Year.x','Entity','Code')]

data <- data[ , !names(data) %in% c('PrimaryKey','Year.x','Entity','Code')]

names(data)[names(data) == 'Year.y'] <- 'Year'

data <- data %>% relocate(Year, .before = Life.Expectancy)
head(data)

data2 <- data %>% drop_na(Life.Expectancy)
write.csv(data2, "LifeExpectancy_merged_R.csv", row.names=FALSE)
