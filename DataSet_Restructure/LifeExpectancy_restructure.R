library(tidyverse)
library(reshape)
library(stringr)
require(dplyr)

#LifeExpectancy
life <- read.csv("LifeExpectancy_world_bank.csv")
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
cause["Entity"][cause["Entity"] == 'Russia'] <- 'Russian Federation'

life$PrimaryKey <- str_c(life$Country.Name ,life$Year)
cause$PrimaryKey <- str_c(cause$Entity,'X',cause$Year)

data <- merge(life, cause, by="PrimaryKey")
head(data)

data <- data[ , !names(data) %in% c('PrimaryKey','Year.x','Entity','Code')]

names(data)[names(data) == 'Year.y'] <- 'Year'

data <- data %>% relocate(Year, .before = Life.Expectancy)
names(data)[names(data) == 'Country.Name'] <- 'Country'
head(data)

merged <- data %>% drop_na(Life.Expectancy)
write.csv(merged, "LifeExpectancy_merged_R.csv", row.names=FALSE)

#Add Status in our dataset
life_WHO <- read.csv("Life Expectancy Data.csv")

#Changing Country names in life expectancy data WHO
life_WHO["Country"][life_WHO["Country"] == 'United States of America'] <- 'United States'
life_WHO["Country"][life_WHO["Country"] == 'United Kingdom of Great Britain and Northern Ireland'] <- 'United Kingdom'
life_WHO["Country"][life_WHO["Country"] == 'Bolivia (Plurinational State of)'] <- 'Bolivia'
life_WHO["Country"][life_WHO["Country"] == "Côte d'Ivoire"] <- "Cote d'Ivoire"
life_WHO["Country"][life_WHO["Country"] ==  'Swaziland'] <- 'Eswatini'
life_WHO["Country"][life_WHO["Country"] == 'Viet Nam'] <- 'Vietnam'

status_dict <- hash::hash(keys = life_WHO$Country, values = life_WHO$Status)

for(i in 1:nrow(merged)) {   
  merged["Status"][i,] <- status_dict[[merged["Country"][i, ]]]
}

write.csv(merged,"LifeExpectancy_statusAdded.csv")