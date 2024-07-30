

#Loading the  Libraries 
library(caTools)
library(tidyr)
library(GGally)
library(dplyr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(gapminder)
library(Amelia)
library(quantmod)
library(ggplot2)


#loading data set
life <- read.csv("C:\\Users\\dell\\Desktop\\EDA life expectency\\LifeExpectancyDataset2.csv")
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
#gives us summary of the dataset


# Renaming the columns on dataset 
life <- life %>%
  rename(
    Life_expectancy = Life.expectancy,
         Meningitis = Deaths...Meningitis...Sex..Both...Age..All.Ages..Number. ,
         Execution = Number.of.executions..Amnesty.International.,
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
         Cirrhosis_and_Liver_deisease = Deaths...Cirrhosis.and.other.chronic.liver.diseases...Sex..Both...Age..All.Ages..Number. ,
         Digestive_disease = Deaths...Digestive.diseases...Sex..Both...Age..All.Ages..Number.,
         Acute_Hepatitis = Deaths...Acute.hepatitis...Sex..Both...Age..All.Ages..Number.,
         Alzheimers = Deaths...Alzheimer.s.disease.and.other.dementias...Sex..Both...Age..All.Ages..Number.,
         Parkinsons = Deaths...Parkinson.s.disease...Sex..Both...Age..All.Ages..Number.
         
         )

# Now again looking at the column names
colnames(life)


# Structure check of the variables
str(life)

#Eliminating duplicate values
length(unique(life$Country))
#can't help in prediction too many unique values


# Data pre-processing:

# find the missing value by using visualization

missmap(life, main="Life_expectancy - Finding Missing Data",
        col=c("red", "black"), legend = F)

# checking for missing values
colSums(is.na(life))

as.data.frame(colSums(is.na(life)))

# Missing values imputation by median as these columns have many outliers.

boxplot(life$infantMortality)
median(life$infantMortality, na.rm = T)
life$infantMortality[is.na(life$infantMortality)] <- 22

boxplot(life$Population)
median(life$Population, na.rm = T)
life$Population[is.na(life$Population)] <- 7955892

boxplot(life$GDP)
median(life$GDP, na.rm = T)
life$GDP[is.na(life$GDP)] <- 23.2

boxplot(life$Meningitis)
median(life$Meningitis, na.rm = T)
life$Meningitis[is.na(life$Meningitis)] <- 150

boxplot(life$Neoplasms)
median(life$Neoplasms, na.rm = T)
life$ Neoplasms[is.na(life$ Neoplasms)] <- 6811.5

boxplot(life$Fire)
median(life$Fire, na.rm = T)
life$Fire [is.na(life$Fire )] <- 164

boxplot(life$Malaria)
median(life$Malaria, na.rm = T)
life$Malaria[is.na(life$Malaria)] <- 0


boxplot(life$Drowning)
median(life$Drowning, na.rm = T)
life$Drowning[is.na(life$Drowning)] <- 262


boxplot(life$Interpersonal_Violence)
median(life$Interpersonal_Violence, na.rm = T)
life$Interpersonal_Violence[is.na(life$Interpersonal_Violence)] <- 360.5


boxplot(life$HIV_AIDs)
median(life$HIV_AIDs, na.rm = T)
life$HIV_AIDs [is.na(life$HIV_AIDs )] <- 244


boxplot(life$Drugs_Use_Disorders)
median(life$Drugs_Use_Disorders, na.rm = T)
life$Drugs_Use_Disorders[is.na(life$Drugs_Use_Disorders)] <- 28


boxplot(life$Tuberculosis)
median(life$Tuberculosis, na.rm = T)
life$Tuberculosis[is.na(life$Tuberculosis)] <- 570.5


boxplot(life$Road_injuries)
median(life$Road_injuries, na.rm = T)
life$Road_injuries[is.na(life$Road_injuries)] <- 1250


boxplot(life$Maternal_Disorders)
median(life$Maternal_Disorders, na.rm = T)
life$Maternal_Disorders[is.na(life$Maternal_Disorders)] <- 74


boxplot(life$Lower_respiratory_infections)
median(life$Lower_respiratory_infections, na.rm = T)
life$Lower_respiratory_infections[is.na(life$Lower_respiratory_infections)] <- 3385.5


boxplot(life$Neonatal_disorders)
median(life$Neonatal_disorders, na.rm = T)
life$Neonatal_disorders[is.na(life$Neonatal_disorders)] <- 1288


boxplot(life$Alcohol_use_disorders)
median(life$Alcohol_use_disorders, na.rm = T)
life$Alcohol_use_disorders[is.na(life$Alcohol_use_disorders)] <- 100


boxplot(life$Exposure_to_force_of_Nature)
median(life$Exposure_to_force_of_Nature, na.rm = T)
life$Exposure_to_force_of_Nature[is.na(life$Exposure_to_force_of_Nature)] <- 0


boxplot(life$Diarrheal_diseases)
median(life$Diarrheal_diseases, na.rm = T)
life$Diarrheal_diseases[is.na(life$Diarrheal_diseases)] <- 536.5


boxplot(life$Environmental_heat_cold)
median(life$Environmental_heat_cold, na.rm = T)
life$Environmental_heat_cold[is.na(life$Environmental_heat_cold)] <- 34


boxplot(life$Nutritional_Deficiences)
median(life$Nutritional_Deficiences, na.rm = T)
life$Nutritional_Deficiences[is.na(life$Nutritional_Deficiences)] <- 165


boxplot(life$Self_Harm)
median(life$Self_Harm, na.rm = T)
life$Self_Harm[is.na(life$Self_Harm)] <- 634


boxplot(life$Confilct_and_Terrorism)
median(life$Confilct_and_Terrorism, na.rm = T)
life$Confilct_and_Terrorism[is.na(life$Confilct_and_Terrorism)] <- 1


boxplot(life$Diabetes_mellitus)
median(life$Diabetes_mellitus, na.rm = T)
life$Diabetes_mellitus[is.na(life$Diabetes_mellitus)] <- 1373.5


boxplot(life$Poisonings)
median(life$Poisonings, na.rm = T)
life$ Poisonings[is.na(life$ Poisonings)] <- 72


boxplot(life$Protein_Malnutrition)
median(life$Protein_Malnutrition, na.rm = T)
life$Protein_Malnutrition[is.na(life$Protein_Malnutrition)] <- 128.5


boxplot(life$Terrorism)
median(life$Terrorism, na.rm = T)
life$Terrorism[is.na(life$Terrorism)] <- 4


boxplot(life$Cardiovascular_disease)
median(life$Cardiovascular_disease, na.rm = T)
life$Cardiovascular_disease[is.na(life$Cardiovascular_disease)] <- 13994.5


boxplot(life$Chronic_Kidney_disease)
median(life$Chronic_Kidney_disease, na.rm = T)
life$Chronic_Kidney_disease[is.na(life$Chronic_Kidney_disease)] <- 1097.5


boxplot(life$Chronic_respiratory_disease)
median(life$Chronic_respiratory_disease, na.rm = T)
life$Chronic_respiratory_disease [is.na(life$Chronic_respiratory_disease )] <- 1998


boxplot(life$Cirrhosis_and_Liver_deisease)
median(life$Cirrhosis_and_Liver_deisease, na.rm = T)
life$Cirrhosis_and_Liver_deisease[is.na(life$Cirrhosis_and_Liver_deisease)] <- 1538


boxplot(life$Digestive_disease)
median(life$Digestive_disease, na.rm = T)
life$Digestive_disease[is.na(life$Digestive_disease)] <- 2850


boxplot(life$Acute_Hepatitis)
median(life$Acute_Hepatitis, na.rm = T)
life$Acute_Hepatitis[is.na(life$Acute_Hepatitis)] <- 24


boxplot(life$Alzheimers)
median(life$Alzheimers, na.rm = T)
life$Alzheimers [is.na(life$Alzheimers )] <- 812


boxplot(life$Parkinsons)
median(life$Parkinsons, na.rm = T)
life$Parkinsons [is.na(life$Parkinsons )] <- 203

life$Execution [is.na(life$Execution )] <- 0


# Rechecking the missing values
colSums(is.na(life))
colSums(is.na(life)) / nrow(life)


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

#Histogram for life expectency

hist(life$Life_expectancy, col="darkmagenta", main="Life Expectancy Data")

#Population Vs Life Expectancy 

population <- plot_ly(data = life,x=~Life_expectancy,
                      y = ~Population, color=~Country,
                      text = ~paste("Population", Population ,'$<br>Country:',Country),
                      marker=list(size=5))

population

#GDP vs Life Expectancy of the countryeach county
gdp_country <- plot_ly(life,x=~Life_expectancy,
                       y = ~GDP, color=~Country,
                       text = ~paste("GDP", GDP ,'$<br>Country:',Country),
                       frame = ~Year, type='scatter',mode='markers') %>%
  layout(title="Life Expectancy vs GDP of countries in every year")

gdp_country <- gdp_country %>% layout(
  xaxis = list(
    type = "log"
  )
)

gdp_country

#Plot of malaria for each country with year

bp <- plot_ly(life, x=~Life_expectancy,y=~Malaria, color = ~as.factor(Year), 
              type="bar",boxpoints = "all", jitter = 0.4,pointpos = -1.8) %>% 
  layout(title = "Malaria ", xaxis = list(title = "Life Expectancy  and Malaria", showgrid = F))

bp

#Infants death of each country
infants_country <- plot_ly(life, x=~Life_expectancy,
                           y = ~infantMortality, color=~Country,
                           text = ~paste("Infant Mortality", infantMortality ,'$<br>Country:',Country),
                           frame = ~Year, type='scatter',mode='markers',
                           marker = list(size=15)) 

infants_country <- infants_country %>% layout(title="Life Expectancy Vs Infant Deaths based on Country Year Wise")

infants_country <- infants_country %>% layout(
  xaxis = list(
    type = "log"
  )
)

infants_country



# Subplots of Life Expectancy with other diseases


fig1 <- plot_ly(life,x = ~Life_expectancy, y = ~Acute_Hepatitis, type = 'scatter', 
                mode = 'lines+markers') 


fig2 <- plot_ly(life,x = ~Life_expectancy, y = ~Diabetes_mellitus, type = 'scatter', 
                mode = 'lines+markers') 


fig3 <- plot_ly(life,x = ~Life_expectancy, y = ~HIV_AIDs, type = 'scatter', 
                mode = 'lines+markers') 
fig4 <- plot_ly(life,x = ~Life_expectancy, y = ~Tuberculosis, type = 'scatter', 
                mode = 'lines+markers') 

fig <- subplot(fig1, fig2, fig3,fig4, nrows = 2) %>% 
  
  layout(title = list(text = " Life Expectancy Subplots"),
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff')) 

annotations = list( 
  list( 
    x = 0.2,  
    y = 1.0,  
    text = "Acute Hepatitis",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.8,  
    y = 1,  
    text = "Diabetes",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.2,  
    y = 0.45,  
    text = "HIV",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list( 
    x = 0.8,  
    y = 0.45,  
    text = "Tuberculosis",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ))

fig <- fig %>%layout(annotations = annotations) 
#options(warn = -1)
fig


# Life expectency of each country

p <- ggplot(life,aes(Year, Life_expectancy, frame = Country,
                     colour=Country)) +
  geom_line(aes(linetype=Country))

fig <- ggplotly(p) %>%
  layout(
    title = "countrywise Life Expectancy ",
    yaxis = list(
      title = "Life Expectancy",
      
      zeroline = F,
      tickprefix = "$"
    ),
    xaxis = list(
      title = "Year",
      zeroline = F, 
      showgrid = F
    )
  ) %>% 
  
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Country "
    )
  )

fig

#Splitting the data into train & test

set.seed(180)
split <- sample.split(life$Life_expectancy, SplitRatio = 0.75)
split

table(split)

training <- subset(life, split==TRUE)
nrow(training)
testing <- subset(life, split==FALSE)
nrow(testing)

