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

life <- read.csv("Life Expectancy Data.csv")
head(life)

colnames(life)
describe_data <- summary(life)
describe_data

#Plotting missing values
missing.values <- life %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing values", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


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


#Population Vs Life Expectancy of each country
population <- plot_ly(data = life,x=~Life.expectancy,
                      y = ~Population, color=~Country,
                      text = ~paste("Population", Population ,'$<br>Country:',Country),
                      marker=list(size=5)) %>%
  layout(title="Life Expectancy vs Population of countries in every year")

pop_country <- population %>% layout(
  xaxis = list(
    type = "log"
  )
)
pop_country

#GDP vs Life Expectancy of each country

gdp_country <- plot_ly(data = life,x=~Life.expectancy,
                       y = ~GDP, color=~Country,
                       text = ~paste("GDP", GDP ,'$<br>Country:',Country),
                       type='scatter',mode='markers') %>%
  layout(title="Life Expectancy vs GDP of countries in every year")

gdp_country <- gdp_country %>% layout(
  xaxis = list(
    type = "log"
  )
)

gdp_country

#Plot of diphtheria for each country with year
diph <- plot_ly(life, x=~Life.expectancy,
                           y = ~Diphtheria, color=~Country,
                           text = ~paste("Deaths due to Diphtheria", Diphtheria ,'$<br>Country:',Country),
                           type='scatter',mode='markers',
                           marker = list(size=5)) 

diph <- diph %>% layout(title="Life Expectancy Vs Deaths due to Diphtheria")

diph <- diph %>% layout(
  xaxis = list(
    type = "log"
  )
)

diph


#Infants death of each country
infants_country <- plot_ly(life, x=~Life.expectancy,
                           y = ~infant.deaths, color=~Country,
                           text = ~paste("Infant Mortality", infant.deaths ,'$<br>Country:',Country),
                          type='scatter',mode='markers',
                           marker = list(size=15)) 

infants_country <- infants_country %>% layout(title="Life Expectancy Vs Infant Deaths based on Country Year Wise")

infants_country <- infants_country %>% layout(
  xaxis = list(
    type = "log"
  )
)

infants_country
