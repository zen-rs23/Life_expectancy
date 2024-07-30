## Packages required
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(plotly)
library(tidyverse)
library(cowplot)
library(psych)
library(lattice)
library(xtable)
library(plyr); library(dplyr)
library(gridExtra)
library(WVPlots)

#Reading csv file
life_expectancy_data <- read.csv(
  "C:\\Users\\krohi\\OneDrive\\Desktop\\Life_expectancy_status.csv")
head(life_expectancy_data, 5)

#Dimensions : Gives numbers of rows and columns
dim(life_expectancy_data)


# Structure of dataset
str(life_expectancy_data)

#statistical summary of the variables
summary(life_expectancy_data)

#Check for missing values
colSums(is.na(life_expectancy_data))

# Select numeric variables for calculating mean
life_expectancy_data_num <- select(life_expectancy_data,-c(1,2,3,4))

#Calculate means of all the numeric variables
colMeans(life_expectancy_data_num, na.rm = TRUE)

#Impute missing values in numeric variables with mean
for(i in 5:ncol(life_expectancy_data)) {
  life_expectancy_data[ , i][is.na(life_expectancy_data[ , i])] <- mean(life_expectancy_data[ , i], na.rm=TRUE)
}
summary(life_expectancy_data) 


# We can see that now the data set has no missing values
colSums(is.na(life_expectancy_data))


dim(life_expectancy_data)

#Plotting box plots of life expectancy to understand outliers
boxplot(life_expectancy_data$Life.expectancy, xlab="Life Expectancy")

outliers <- boxplot(life_expectancy_data$Life.expectancy, plot=FALSE)$out

life_expectancy_data<- life_expectancy_data[-which(life_expectancy_data$Life.expectancy %in% outliers),]

dim(life_expectancy_data)

#correlation between percentage expenditure and life expectancy
life_expectancy_vs_percenntage_expenditure <-  ggplot(life_expectancy_data, aes(percentage.expenditure, Life.expectancy)) + 
  geom_jitter(color = "red", alpha = 0.5) + theme_light()

life_expectancy_vs_Total_expenditure  <- ggplot(life_expectancy_data, aes(Total.expenditure, Life.expectancy)) +
  geom_jitter(color = "Green", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_percenntage_expenditure, life_expectancy_vs_Total_expenditure) 
title <- ggdraw() + draw_label("Correlation between Health expenditure and life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

library(plotly)
life_expectancy_vs_Hepatitis_B <- ggplot(life_expectancy_data, aes(Hepatitis.B, Life.expectancy)) + 
  geom_jitter(color = "violet", alpha = 0.5) + theme_light()

life_expectancy_vs_Diphtheria  <- ggplot(life_expectancy_data, aes(Diphtheria, Life.expectancy)) +
  geom_jitter(color = "magenta", alpha = 0.5) + theme_light()

life_expectancy_vs_Polio  <- ggplot(life_expectancy_data, aes(Polio, Life.expectancy)) + geom_jitter(color = "pink", alpha = 0.5) + theme_grey()

p <- plot_grid(life_expectancy_vs_Hepatitis_B, life_expectancy_vs_Diphtheria, life_expectancy_vs_Polio ) 
title <- ggdraw() + draw_label("Correlation between Immunizations and life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))



#correlation between measles and life expectancy
life_expectancy_vs_Measles  <- plot_ly(data = life_expectancy_data, x = ~Measles , y = ~Life.expectancy,
                                       marker = list(size = 10,
                                                     color =  'rgba(221,160,221, .3)',
                                                     line = list(color = 'rgba(255, 0, 38, 0.2)',
                                                                 width = 2)))
life_expectancy_vs_Measles  <- life_expectancy_vs_Measles  %>% layout(title = 'Scatter Plot: Life Expectancy vs Measles',
                                                                      yaxis = list(zeroline = FALSE),
                                                                      xaxis = list(zeroline = FALSE))

life_expectancy_vs_Measles 


#correlation between alcohol and life expectancy
life_expectancy_vs_Alcohol  <- plot_ly(data = life_expectancy_data, x = ~Alcohol , y = ~Life.expectancy,
                                       marker = list(size = 10,
                                                     color = 'rgba(152, 215, 182, .5)',
                                                     line = list(color = 'rgba(0, 0, 0, 0)',
                                                                 width = 2)))
life_expectancy_vs_Alcohol  <- life_expectancy_vs_Alcohol  %>% layout(title = 'Scatter Plot: Life Expectancy vs Alcohol ',
                                                                      yaxis = list(zeroline = FALSE),
                                                                      xaxis = list(zeroline = FALSE))

life_expectancy_vs_Alcohol 

#correlation between BMI and life expectancy
life_expectancy_vs_BMI <- plot_ly(data = life_expectancy_data, x = ~BMI, y = ~Life.expectancy,
                                  marker = list(size = 10,
                                                color = 'rgba(255,182,193, .9)',
                                                line = list(color = 'rgba(255, 0, 38, 0.2)',
                                                            width = 2)))
life_expectancy_vs_BMI <- life_expectancy_vs_BMI %>% layout(title = 'Scatter Plot: Life Expectancy vs BMI',
                                                            yaxis = list(zeroline = FALSE),
                                                            xaxis = list(zeroline = FALSE))

life_expectancy_vs_BMI

library(plotly)
life_expectancy_vs_under_five_deaths  <- ggplot(life_expectancy_data, aes(under.five.deaths, Life.expectancy)) + geom_jitter(color = "pink", alpha = 0.5) + theme_grey()

p <- plot_grid(life_expectancy_vs_under_five_deaths)
title <- ggdraw() + draw_label("Correlation between Under five deaths and life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


#correlation between GDP and lif expectancy
life_expectancy_vs_GDP  <- ggplot(life_expectancy_data, aes(GDP, Life.expectancy)) +
  geom_jitter(color = "dark green", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_GDP) 
title <- ggdraw() + draw_label("Correlation between GDP vs Life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

#correlation between thinness and life expectancy
life_expectancy_vs_thinness_1_19_years  <- ggplot(life_expectancy_data, aes(thinness..1.19.years, Life.expectancy)) +
  geom_jitter(color = "blue", alpha = 0.5) + theme_light()

life_expectancy_vs_thinness_5_9_years  <- ggplot(life_expectancy_data, aes(thinness.5.9.years, Life.expectancy)) +
  geom_jitter(color = "orange", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_thinness_1_19_years, life_expectancy_vs_thinness_5_9_years) 
title <- ggdraw() + draw_label("Correlation between Thinness vs Life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

library(plotly)
life_expectancy_vs_Income_composition_of_resources <- plot_ly(data = life_expectancy_data, x = ~Income.composition.of.resources , y = ~Life.expectancy,
                                                              marker = list(size = 10,
                                                                            color = 'rgba(181, 201, 253, .9)',
                                                                            line = list(color = 'rgba(255, 0, 38, 0.2)',
                                                                                        width = 2)))
life_expectancy_vs_Income_composition_of_resources  <- life_expectancy_vs_Income_composition_of_resources %>% layout(title = 'Scatter Plot: Life Expectancy vs Income composition of resources',
                                                                                                                     yaxis = list(zeroline = FALSE),
                                                                                                                     xaxis = list(zeroline = FALSE))

life_expectancy_vs_Income_composition_of_resources
########################################

###### Modelling######

########################################
#we will split the data into train and test for model building
n_train <- round(0.8 * nrow(life_expectancy_data))
train_indices <- sample(1:nrow(life_expectancy_data), n_train)
train_data <- life_expectancy_data[train_indices, ]
test_data <- life_expectancy_data[-train_indices, ]


#first model
formula1 <- as.formula("Life.expectancy ~ Alcohol + percentage.expenditure + Hepatitis.B + Measles +  BMI + under.five.deaths + Polio+ Total.expenditure + Diphtheria  + thinness..1.19.years + thinness.5.9.years + GDP + Income.composition.of.resources")
model1 <- lm(formula1, data = train_data)
summary(model1)


summary(model1)$coefficient


#second model
#dropping insignificant variables like measles, percentage.expenditure, Hepatitis.B, Under.five.deaths, Thinness.5.9.years
formula2 <- as.formula("Life.expectancy ~  Alcohol +  Diphtheria  +  BMI +  Polio + Total.expenditure + thinness..1.19.years +  Income.composition.of.resources")
model2 <- lm(formula2, data = train_data)
summary(model2)

r_sq1 <- summary(model1)$r.squared
prediction1 <- predict(model1, newdata = test_data)
residuals1 <- test_data$Life.expectancy - prediction1
rmse1 <- sqrt(mean(residuals1^2, na.rm=TRUE))

r_sq2 <- summary(model2)$r.squared
prediction2 <- predict(model2, newdata = test_data)
residuals2 <- test_data$Life.expectancy - prediction2
rmse2 <- sqrt(mean(residuals2^2, na.rm=TRUE))


#Comparing the models
print(paste0("R-squared for first model:", round(r_sq1, 4)))
## [1] "R-squared for first model:0.7571"
print(paste0("R-squared for second model: ", round(r_sq2, 4)))
## [1] "R-squared for second model: 0.7486"
print(paste0("RMSE for first model: ", round(rmse1, 2)))
## [1] "RMSE for first model: 5.1"
print(paste0("RMSE for second model: ", round(rmse2, 2)))
## [1] "RMSE for second model: 5.09"


#The confidence interval of the model coefficient can be extracted as follow:

confint(model2, level=0.95)

#prediction
test_data$prediction <- predict(model2, newdata = test_data)
ggplot(test_data, aes(x = prediction, y = Life.expectancy)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")


#residuals vs linear model prediction
test_data$residuals <- test_data$Life.expectancy - test_data$prediction
ggplot(data = test_data, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "purple", alpha = 0.7) + geom_hline(yintercept = 0, linetype = 4, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")


#histogram for residuals
ggplot(test_data, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "light blue") +
  ggtitle("Histogram of residuals")


GainCurvePlot(test_data, "prediction", "Life.expectancy", "Model2")


sigma(model2)/mean(life_expectancy_data$Life.expectancy)


#test1
XYZ <- data.frame(  Country = "India",
                    Alcohol = 3.07,
                    Diphtheria = 85,
                    BMI = 18.1,
                    Polio = 84,
                    Total.expenditure = 4.69,
                    thinness..1.19.years = 26.8,
                    Income.composition.of.resources = 0.607)
print(paste0("Life expectancy for XYZ: ", round(predict(model2, XYZ), 2)))
