#Loading the  Libraries 
library(caTools)
library(tidyr)
library(GGally)
library(dplyr)
library(plotly)
library(ggplot2)
library(gapminder)
library(Amelia)
library(quantmod)
library(tidyverse)
library(reshape)
library(reshape2)
library(stringr)

life <- read.csv("Life_Expectancy_with_CauseOfDeath.csv")
head(life)

#Checking the correlation the parameters
Corrdata <- life %>% 
  select_if(is.numeric)

#Plotting the correlation among all parameter
ggcorr(Corrdata, 
       method = c("pairwise","pearson"),
       label = T, 
       label_size = 2,
       label_round = 2,
       hjust = 1,
       size = 3, 
       color = "royalblue",
       layout.exp = 5,
       low = "green3", 
       mid = "gray95", 
       high = "darkorange",
       name = "Correlation")
