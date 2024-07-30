# Loading Libraries
library(tidyverse)
library(skimr)
library(reshape)
library(mlbench)
library(reshape)
library(GGally)
library(vip)
library(ISLR)
library(WVPlots)
library(ggstatsplot)
library(hrbrthemes)
library(viridis)
library(ggExtra)
library(plotly)
library(patchwork)
library(tidymodels)
library(mltools)
library(ggplot2)
library(plotly)
library(caret)
library(glmnet)
library(GGally)
library(corrplot)
library(ggstar)
library(leaps)
library(gganimate)
library(corrr)
library(mltools)
library(elasticnet)
library(tidygeocoder)
library(glmnet)
library(DT)
library(broom)
options(repr.plot.width = 8, repr.plot.height = 8)
library(knitr)
library(gridExtra)
library(dplyr)
library(wesanderson)

# Loading the dataset (csv file)
life_data <- read.csv("C:\\Users\\krohi\\OneDrive\\Desktop\\LifeExpectancy_statusAdded.csv")

# Checking for missing values
life_data <- life_data %>%
  filter(!is.na(Life.expectancy))
sum(is.na(life_data))

# sample size
#sample_size = life_data %>% group_by(Year) %>% summarize(num=n())

# Plot
#life_data %>%
 # left_join(sample_size) %>%
#  mutate(myaxis = paste0(Year, "\n", "n=", num)) %>%
#  ggplot( aes(x=myaxis, y=value, fill=Year)) +
#  geom_violin(width=1.4) +
#  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
#  scale_fill_viridis(discrete = TRUE) +
#  theme_ipsum() +
#  theme(
#    legend.position="none",
#    plot.title = element_text(size=11)
#  ) +
#  ggtitle("A Violin wrapping a boxplot") +
#  xlab("")

#Checking the distribution of data for all variables

par(mfrow= c(1,1))
boxplot(life_data$Life.expectancy, 
        main = "Life Expectancy Globally", 
        ylab = "Life Expectancy Years")
par(mfrow = c(2,3))
invisible(lapply(5:10, function(i) boxplot(life_data[, i],
                                           main = colnames(life_data)[i],col="blue")))
par(mfrow = c(2,3))
invisible(lapply(11:16, function(i) boxplot(life_data[, i],
                                            main = colnames(life_data)[i],col="blue")))


par(mfrow = c(2,3))

invisible(lapply(17:22, function(i) boxplot(life_data[, i],
                                            main = colnames(life_data)[i],col="blue")))


par(mfrow = c(2,3))

invisible(lapply(23:28, function(i) boxplot(life_data[, i],
                                            main = colnames(life_data)[i], col="blue")))

par(mfrow = c(2,3))

invisible(lapply(29:34, function(i) boxplot(life_data[, i],
                                            main = colnames(life_data)[i], col="blue")))

par(mfrow = c(2,3))

invisible(lapply(35:40, function(i) boxplot(life_data[, i],
                                            main = colnames(life_data)[i], col="blue")))
par(mfrow = c(2,3))

invisible(lapply(41:46, function(i) boxplot(life_data[, i],
                                            main = colnames(life_data)[i], col="blue")))
par(mfrow = c(2,3))

invisible(lapply(47:52, function(i) boxplot(life_data[, i],
                                            main = colnames(life_data)[i], col="blue")))
par(mfrow = c(2,3))

invisible(lapply(53:56, function(i) boxplot(life_data[, i],
                                            main = colnames(life_data)[i], col="blue")))
#### plotting a correlation matrix between variables
life_cormat <- round(cor(life_data[,5:24],use = "complete.obs"),2)
life_cormat1<-round(cor(life_data[,25:56],use = "complete.obs"),2)

par(mfrow=c(1,1))
# png(file="corr.png", res=200, width=1200, height=1400)
corrplot(life_cormat, method="number",
         tl.cex = 0.5,
         number.cex = 0.5,
         cl.cex = 0.5)

par(mfrow=c(1,1))
# png(file="corr.png", res=200, width=1200, height=1400)
corrplot(life_cormat1, method="number",
         tl.cex = 0.5,
         number.cex = 0.5,
         cl.cex = 0.5)
##### Plotting a scatter plot matrix with density plots

life_data %>%
  ggscatmat(columns = 5:12, color="Status",alpha = 0.7)



# plot all the variables against data, four plots in a grid
par(mfrow = c(2,2))
counter = 0
for (variable in colnames(life_data[, -c(1:4)])) {
  plot(life_data[, variable], life_data$Life.expectancy, main = variable, ylab = "Life.expectancy", xlab = variable)
  counter = counter + 1
  if (counter %% 4 == 0) {
    readline(prompt = "Hit ENTER to show more plots")
  }
}


#################################################################

###################  EDA ########################################


#################################################################

# Life expectancy in developed and developing countries

ggplot(data=life_data,mapping=aes(Year,Life.expectancy,color=Status))+
  geom_point()+
  scale_color_brewer(palette="Accent")+
  geom_smooth(method="lm",se=FALSE)+
  labs(title="Life Expectancy from 2000-2015")+ theme(plot.title = element_text(hjust = 0.5))
       
##Adult Mortality
p<- life_data %>% 
ggplot()+
geom_point(aes(Adult.Mortality, Life.expectancy, alpha=0.5,color=Status)) +
geom_smooth(aes(Adult.Mortality, Life.expectancy))+
         labs(x="Adult Mortality", Y="Life expectancy")+
         theme(legend.position = "none")
       
  ggMarginal(p, type="histogram", fill="slateblue")
       
ggplot(life_data, aes(Year, Adult.Mortality) +
                geom_point()+geom_line())
       
       ###Income composition

p2<- life_data %>% 
ggplot()+
   geom_point(aes(Income.composition.of.resources, Life.expectancy,
                        color=Country,
                        fill=Status,alpha=0.01, size= GDP), shape=21)+
         scale_size(range=c(.1, 6))+
         
         scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
         theme_ipsum() +
         theme(legend.position="bottom") +
         ylab("Life Expectancy") +
         xlab("Income Composition") +
         theme(legend.position = "none")
       ggMarginal(p2, type="density", fill="grey")         
       
       
####Schooling

p3<- life_data %>% 
         ggplot()+
         geom_point(aes(Schooling, Life.expectancy, alpha=0.5,color=Status)) +
         geom_smooth(aes(Schooling, Life.expectancy))+
         labs(x="Schooling", Y="Life expectancy")+ theme_bw()+
         theme(legend.position = "none") 
       
       ggMarginal(p3, type="histogram", fill="slateblue")
       
       
       
#####Traffic Mortality
       
p4<- life_data %>% 
         ggplot()+
         geom_point(aes(Traffic.Mortality, Life.expectancy,colour=Status)) +
         geom_smooth(aes(Traffic.Mortality, Life.expectancy))+
         labs(x="Traffic Mortality", Y="Life expectancy")+
         theme_bw()
       plot(p4)  
       
#### Population
       life_data %>% 
         ggplot()+
         geom_point(aes(Population, Life.expectancy,colour=Status)) +
         geom_smooth(aes(Population, Life.expectancy))+
         labs(x="Population", Y="Life expectancy")+
         theme_bw()
       
#### GDP
       life_data %>% 
         ggplot()+
         geom_point(aes(GDP, Life.expectancy,colour=Status)) +
         geom_smooth(aes(GDP, Life.expectancy))+
         labs(x="GDP", Y="Life expectancy")+
         theme_bw()
       
### Immunisation coverage
       
       par(mfrow = c(2,2))
       
       
p5<- life_data %>% 
         ggplot()+
         geom_point(aes(BMI, Life.expectancy,color=Status)) +
         geom_smooth(aes(BMI, Life.expectancy))+
         labs(x="BMI", Y="Life expectancy")+
         theme_bw()+theme(legend.position = "none")
       plot(p5)  
       
p6<-life_data %>% 
         ggplot()+
         geom_point(aes(Diphtheria, Life.expectancy,color=Status)) +
         geom_smooth(aes(Diphtheria, Life.expectancy))+
         labs(x="Diphtheria", Y="Life expectancy")+
         theme_bw()
       plot(p6)
       
       
p7<-life_data %>% 
         ggplot()+
         geom_point(aes(Measles, Life.expectancy,color=Status)) +
         geom_smooth(aes(Measles, Life.expectancy))+
         labs(x="Measles", Y="Life expectancy")+
         theme_bw() + theme(legend.position = "none")
       plot(p7)
       
       
       
p8<-life_data %>% 
         ggplot()+
         geom_point(aes(Hepatitis.B, Life.expectancy,color=Status)) +
         geom_smooth(aes(Hepatitis.B, Life.expectancy))+
         labs(x="Hepatitis.B", Y="Life expectancy")+
         theme_bw() 
       plot(p8)
       
p9<-life_data %>% 
         ggplot()+
         geom_point(aes(Polio, Life.expectancy,color=Status)) +
         geom_smooth(aes(Polio, Life.expectancy))+
         labs(x="Polio", Y="Life expectancy")+
         theme_bw() 
       plot(p9)
       
       grid.arrange(plot(p9),plot(p6),plot(p8),nrow=3)
       
       
#####HIV.AIDS       
 p10<-life_data %>% 
         ggplot()+
         geom_point(aes(HIV.AIDS, Life.expectancy,color=Status)) +
         geom_smooth(aes(HIV.AIDS, Life.expectancy))+
         labs(x="", Y="Life expectancy")+
         theme_bw() 
plot(p10)
       
       
       