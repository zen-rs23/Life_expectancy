library(tidyverse)
library(reshape)
library(stringr)
library(dplyr)
require(dplyr)

life <- read.csv("LifeExpectancy_merged_R.csv")
life <- life[ , !names(life) %in% c("Number.of.executions..Amnesty.International.")]

colnames(life)
describe_data <- summary(life)
describe_data

#Plotting missing values
library(ggplot2)

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

# Remove column with high nan values
life <- life[ , !names(life) %in% c("Terrorism..deaths.")]
life2 <- drop_na(life)
head(life2)

numeric_cols <- select_if(life, is.numeric)
head(numeric_cols)

#Scatter plot
library(reshape2)
df <- melt(life2 ,  id.vars = 'Year')

#create line plot for each column in data frame
ggplot(df, aes(Year,value)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~value)

#p-value
pVals <- numeric()
for(i in 4:ncol(life2))
  pVals[i] <- t.test(life2[,i])$p.value
pVals

