########################### Life Expectancy ################################
############################## R-PROJECT ###################################



# IMPORTING THE LIBRARIES

library(lmtest)
library(faraway)
library(caTools)
library(tidyr)
library(GGally)
library(car)
library(dplyr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(gapminder)
library(Amelia)
library(quantmod)
library(ggplot2)
library(psych) ### stats package - "psych" ####
library(Amelia)
library(corrplot)
library(RColorBrewer) ## library for colors
library(MLmetrics)


life <- read.csv("C:\\Users\\dell\\Desktop\\New folder (3)\\Life_expectancy_status.csv")

life

# Basic EDA

head(life)
class(life)
names(life)
View(life)

dim(life)
# dim() function returns Total dimension i.e. 
# both the number of rows and column in a dataframe.

nrow(life)
ncol(life)
# We can also use ncol() function to find the number of columns
# and nrow() to find the number of rows separately.

summary(life)

describe(life)
# This function provides more deep dive statistics including
# standard deviation, mean absolute deviation, skew, etc

# Renaming columns 

life <- life %>%
  rename(
    "Life_expectancy" = "Life.expectancy",
    Meningitis = Deaths...Meningitis...Sex..Both...Age..All.Ages..Number. ,
    Neoplasms = Deaths...Neoplasms...Sex..Both...Age..All.Ages..Number. ,
    Fire = Deaths...Fire..heat..and.hot.substances...Sex..Both...Age..All.Ages..Number.,
    Malaria = Deaths...Malaria...Sex..Both...Age..All.Ages..Number. ,
    Drowning = Deaths...Drowning...Sex..Both...Age..All.Ages..Number. ,
    Interpersonal_Violence = Deaths...Interpersonal.violence...Sex..Both...Age..All.Ages..Number. ,
    HIV_AIDs = Deaths...HIV.AIDS...Sex..Both...Age..All.Ages..Number. ,
    Drugs_Use_Disorders = Deaths...Drug.use.disorders...Sex..Both...Age..All.Ages..Number. ,
    Tuberculosis = Deaths...Tuberculosis...Sex..Both...Age..All.Ages..Number. ,
    Road_injuries = Deaths...Road.injuries...Sex..Both...Age..All.Ages..Number. ,
    Maternal_Disorders = Deaths...Maternal.disorders...Sex..Both...Age..All.Ages..Number.,
    Lower_respiratory_infections = Deaths...Lower.respiratory.infections...Sex..Both...Age..All.Ages..Number. ,
    Neonatal_disorders = Deaths...Neonatal.disorders...Sex..Both...Age..All.Ages..Number. ,
    Alcohol_use_disorders = Deaths...Alcohol.use.disorders...Sex..Both...Age..All.Ages..Number.,
    Exposure_to_force_of_Nature = Deaths...Exposure.to.forces.of.nature...Sex..Both...Age..All.Ages..Number.,
    Diarrheal_diseases = Deaths...Diarrheal.diseases...Sex..Both...Age..All.Ages..Number.,
    Environmental_heat_cold= Deaths...Environmental.heat.and.cold.exposure...Sex..Both...Age..All.Ages..Number. ,
    Nutritional_Deficiences = Deaths...Nutritional.deficiencies...Sex..Both...Age..All.Ages..Number. ,
    Self_Harm = Deaths...Self.harm...Sex..Both...Age..All.Ages..Number. ,
    Confilct_and_Terrorism = Deaths...Conflict.and.terrorism...Sex..Both...Age..All.Ages..Number. ,
    Diabetes_mellitus = Deaths...Diabetes.mellitus...Sex..Both...Age..All.Ages..Number. ,
    Poisonings = Deaths...Poisonings...Sex..Both...Age..All.Ages..Number. ,
    Protein_Malnutrition = Deaths...Protein.energy.malnutrition...Sex..Both...Age..All.Ages..Number. ,
    Terrorism = Terrorism..deaths. ,
    Cardiovascular_disease = Deaths...Cardiovascular.diseases...Sex..Both...Age..All.Ages..Number.,
    Chronic_Kidney_disease = Deaths...Chronic.kidney.disease...Sex..Both...Age..All.Ages..Number.,
    Chronic_respiratory_disease = Deaths...Chronic.respiratory.diseases...Sex..Both...Age..All.Ages..Number.,
    Cirrhosis_and_Liver_disease = Deaths...Cirrhosis.and.other.chronic.liver.diseases...Sex..Both...Age..All.Ages..Number. ,
    Digestive_disease = Deaths...Digestive.diseases...Sex..Both...Age..All.Ages..Number.,
    Acute_Hepatitis = Deaths...Acute.hepatitis...Sex..Both...Age..All.Ages..Number.,
    Alzheimers = Deaths...Alzheimer.s.disease.and.other.dementias...Sex..Both...Age..All.Ages..Number.,
    Parkinsons = Deaths...Parkinson.s.disease...Sex..Both...Age..All.Ages..Number.
  )

# Now again looking at the column names
colnames(life)

ncol(life)

# Structure check of the variables
str(life)

length(unique(life$Country))
#can't help in prediction too many unique values


# Data pre-processing:

# checking for missing values
colSums(is.na(life))
colSums(is.na(life)) / nrow(life)

# find the missing value by using visualization

missmap(life, main="Life Expectancy - Finding Missing Data",
        col=c("red", "black"), legend = F)

# GDP HAS SOME MISSING VALUES AVAILABLE IN THE DATASET.
# Terrorism has a lot of missing values present in it.

# Remove useless columns
life <- life[,-c(1)]
life <- life[,-c(7)]
life <- life[,-c(30)] # Terrorism column removed


#Dropping null values.
life <- life %>% 
  drop_na()


#now checking again for missing values
colSums(is.na(life))

# Checking for the presence of outliers in variables using boxplot.

boxplot(life$Life_expectancy) 
quantile(life$Life_expectancy, seq(0,1,0.01))
life$Life_expectancy <- ifelse(life$Life_expectancy<45,45, life$Life_expectancy)
boxplot(life$Life_expectancy)

# The average life expectency of all countries combined is at the average of 70.



#Checking the correlation the parameters
#Corrdata <- life %>% 
#select_if(is.numeric)

#Plotting the correlation among all parameter
#ggcorr(Corrdata, 
# method = c("pairwise","pearson"),
# label = T, 
#  label_size = 2,
#  label_round = 2,
#   hjust = 1,
# size = 3, 
#color = "royalblue",
# layout.exp = 5,
#low = "green3", 
#   mid = "gray95", 
#  high = "darkorange",
# name = "Correlation")


# Data visualizations : Univariate analysis & Bivariate analysis

################## Univariate Analysis ##################
# Multiple Continuous Variables


Histogram <- c("Life_expectancy","GDP","infantMortality",
               "Meningitis","Neoplasms","Fire","Malaria",
               "Drowning", "Interpersonal_Violence" ,"HIV_AIDs", "Drugs_Use_Disorders", 
               "Tuberculosis", "Road_injuries" ,"Maternal_Disorders","Lower_respiratory_infections" ,"Neonatal_disorders",
               "Alcohol_use_disorders", "Exposure_to_force_of_Nature","Diarrheal_diseases" ,"Environmental_heat_cold",
               "Nutritional_Deficiences" ,"Self_Harm" ,"Confilct_and_Terrorism","Diabetes_mellitus","Poisonings",
               "Protein_Malnutrition" ,"Cardiovascular_disease" ,"Chronic_Kidney_disease" , "Chronic_respiratory_disease",
               "Cirrhosis_and_Liver_disease" ,"Digestive_disease" ,"Acute_Hepatitis","Alzheimers" ,"Parkinsons")

par(mfrow=c(2,2))

# Using loops to create the histograms for each column   

for (ColumnName in Histogram ){
  hist(life[,(ColumnName)], main=paste('Plot :', ColumnName), xlab = paste("Histogram of",ColumnName),
       col=brewer.pal(8,"Spectral"))}


# Multiple Categorical Variables

ColsForBar <- c("Status")

# looping to create the Bar-Plots for each column\

for (ColumnName in ColsForBar){
  barplot(table(life[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Accent"))}

#################### Bivariate Analysis ########################

# Relationship between target variable and predictors variables
# Categorical vs Continuous ---  Box Plot

# Categorical vs Continuous analysis-- Boxplot

par(mfrow=c(2,2))

ColsForBar <-c("Status")

for(box_cols in ColsForBar){
  boxplot(Life_expectancy~life[  ,c(box_cols)], data=life  , main=paste('Boxplot of :',box_cols),col=brewer.pal(8,"Accent"))
}

# the average life expectancy of people in developed country is close to 80 whereas 
# the developing countries has life expectancy is near to 70.

### Statistical Tests ###

# Strength of Relationship between predictor and target variable
# Continuous Vs Continuous ---- Correlation test
# Continuous Vs Categorical---- ANOVA test

###################### ANOVA TEST ##############################
# Continuous vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)

# Small P-Value < 5% - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value > 5% - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

colsforanova<-c("Status") 

for(anovacols in colsforanova){
  anovaresult=summary(aov(Life_expectancy ~ life[,c(anovacols)],data=life))
  print(colsforanova)
  print(anovaresult)}

## Good predictor variables - "Status"-- low p-value
## so we reject the null hypothesis.


###################### Correlation test ########################

# Continuous Vs Continuous ---- Correlation test

# Correlation using Heatmap visualization

ContinuousCols <- c("Life_expectancy","GDP","infantMortality",
                    "Meningitis","Neoplasms","Fire","Malaria",
                    "Drowning", "Interpersonal_Violence" ,"HIV_AIDs", "Drugs_Use_Disorders", 
                    "Tuberculosis", "Road_injuries" ,"Maternal_Disorders","Lower_respiratory_infections" ,"Neonatal_disorders",
                    "Alcohol_use_disorders", "Exposure_to_force_of_Nature","Diarrheal_diseases" ,"Environmental_heat_cold",
                    "Nutritional_Deficiences" ,"Self_Harm" ,"Confilct_and_Terrorism","Diabetes_mellitus","Poisonings",
                    "Protein_Malnutrition" ,"Cardiovascular_disease" ,"Chronic_Kidney_disease" , "Chronic_respiratory_disease",
                    "Cirrhosis_and_Liver_disease" ,"Digestive_disease" ,"Acute_Hepatitis","Alzheimers" ,"Parkinsons")

CorrData1 <- corrplot(cor(life[,ContinuousCols ], use = "complete.obs"))
CorrData1

# Final columns which has high correlation with the target variable

CorrData <- cor(life[,ContinuousCols ], use = "complete.obs")

names(CorrData[,'Life_expectancy'])

names(CorrData[,'Life_expectancy'][abs(CorrData[,'Life_expectancy'])>0.15])

#"GDP" "infantMortality","Meningitis","Malaria","HIV_AIDs","Maternal_Disorders" 
#"Diarrheal_diseases","Nutritional_Deficiences","Protein_Malnutrition""Alzheimers"---  are good variables. 


# Continuous Vs Continuous --- Scatter plot #

#  LIFE EXPECTANCY VS GDP
ContinuousCols<-c("Life_expectancy","GDP" )
par(mfrow=c(1,1))
plot(life[,ContinuousCols],col='blue')

#  LIFE EXPECTANCY VS infantMortality
ContinuousCols<-c("Life_expectancy","infantMortality")
par(mfrow=c(1,1))
plot(life[,ContinuousCols],col='blue')

#  LIFE EXPECTANCY VS Meningitis
ContinuousCols<-c("Life_expectancy","Meningitis")
par(mfrow=c(1,1))
plot(life[,ContinuousCols],col='blue')


#  LIFE EXPECTANCY VS HIV_AIDs
ContinuousCols<-c("Life_expectancy","HIV_AIDs")
par(mfrow=c(1,1))
plot(life[,ContinuousCols],col='blue')

#  LIFE EXPECTANCY VS Nutritional_Deficiences
ContinuousCols<-c("Life_expectancy","Nutritional_Deficiences")
par(mfrow=c(1,1))
plot(life[,ContinuousCols],col='blue')

#  LIFE EXPECTANCY VS Protein_Malnutrition
ContinuousCols<-c("Life_expectancy","Protein_Malnutrition")
par(mfrow=c(1,1))
plot(life[,ContinuousCols],col='blue')

#  LIFE EXPECTANCY VS Alzheimers
ContinuousCols<-c("Life_expectancy","Alzheimers" )
par(mfrow=c(1,1))
plot(life[,ContinuousCols],col='blue')


# Feature selection for model:

##Potential predictors are ---
#"GDP" "infantMortality","Meningitis","Malaria","HIV_AIDs","Maternal_Disorders" 
#"Diarrheal_diseases","Nutritional_Deficiences","Protein_Malnutrition""Alzheimers"

##### Encoding concept #####
#there is "Status" for which we have to do encoding as there are char variable.

life$Status <- as.factor(life$Status)

str(life)

# Splitting the data into train & test

set.seed(150)
split <- sample.split(life$Life_expectancy, SplitRatio = 0.70)
split

table(split)

training <- subset(life, split==TRUE)
nrow(training)
testing <- subset(life, split==FALSE)
nrow(testing)

############################################################

# Model Building: 
# Building the Linear Regression model with training dataset
# linear regression model function : lm(dv~., data=training)

lin_regession <- lm(Life_expectancy~., data=training)
lin_regession
summary(lin_regession)

# there are some variable which is not statically significant, 
# hence, we have to remove this.

lin_regession1 <- lm(Life_expectancy~.-Status-GDP-infantMortality-Meningitis-Malaria-HIV_AIDs-Maternal_Disorders-Diarrheal_diseases-Nutritional_Deficiences-Protein_Malnutrition-Alzheimers, data=training)
lin_regession1
summary(lin_regession1)

###############################
# Multiple R-squared: 0.9677	
# Adjusted R-squared: 0.9657
###############################

# The fitness of model is good

###########################################################################

# Predictions : predict model by using test dataset.

linear_pred <- predict(lin_regession1, newdata = testing)
linear_pred 

# Combined actual Life Expectancy and the predicted Life Expectancy

linear_pred_cbind <- cbind(testing$Life_expectancy, linear_pred)
linear_pred_cbind

# Assumption Tests of Linear Regression model --

#### Autocorrelation ####
# we have to do durbin Watson test, if value falls between 0 and 4,
# there is no autocorrelation, any value which is less then 0 or more than 4,
# will consider autocorrelation

dwtest(lin_regession1)
# DW = 0.34648 - hence, no Autocorrelation found


# MAPE and MDAPE:

# Mean absolute percentage error (MAPE)

MeanAPE <- mean(abs((testing$Life_expectancy-linear_pred)/testing$Life_expectancy)) * 100
MeanAPE

# Median absolute percentage error (MDAPE)

MedianAPE <- median(abs((testing$Life_expectancy-linear_pred)/testing$Life_expectancy)) * 100
MedianAPE

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

#######################################################
# Multiple R-squared:  0.843	
# Adjusted R-squared:  0.8421 
# Mean Absolute Percentage Error (MAPE) : 2.00632 
# Median Absolute Percentage Error (MDAPE) : 1.05342
# Mean Accuracy of Linear Regression Model :  97.9936
# Median Accuracy of Linear Regression Model :  98.9465
#######################################################





