# Life Expectancy Prediction Using Linear Regression

## Introduction
Life expectancy is a critical indicator of a country's health status and overall well-being. Understanding the factors that influence life expectancy can aid policymakers in implementing effective health strategies. This analysis aims to identify the predicting variables affecting life expectancy and determine the overall average life expectancy across different countries.

## Research Questions
1. What are the predicting variables affecting life expectancy?
2. What is the overall average life expectancy in most countries?
3. Do developed countries have higher life expectancy than developing countries?
4. How do lifestyle choices, such as diet, exercise, smoking, and alcohol consumption, impact life expectancy?
5. Does population size affect life expectancy?
6. What is the impact of immunization on life expectancy?
7. How do different diseases (communicable and non-communicable) affect life expectancy?
8. How do infant and adult mortality rates influence life expectancy?
9. What is the influence of economic factors like GDP per capita on life expectancy?

## Methodology

### Data Collection
The primary dataset is obtained from the World Bank Open Data, including metrics such as life expectancy at birth, infant mortality rate, population, and GDP per capita. Additional data on causes of death are sourced from "Our World in Data."

### Data Cleaning
Missing data is handled by eliminating features with more than 50% missing values and imputing other missing values using mean or forward fill methods.

### Exploratory Data Analysis (EDA)
EDA includes visual and statistical analysis to identify trends and correlations. Heatmaps and distribution plots are used to visualize life expectancy across countries and the relationship between different variables.

### Feature Selection
Pearson correlation coefficient is used to identify and remove redundant features, ensuring the robustness of the machine learning model.

## Model Training and Validation

### Linear Regression
A linear regression model is developed to predict life expectancy using the selected features. The model is trained using a subset of the data and validated using multiple metrics, including R-squared and adjusted R-squared values.

### Evaluation Metrics
- **R-Squared:** Indicates the proportion of variance in the dependent variable explained by the independent variables.
  - Multiple R-squared: 0.9605
  - Adjusted R-squared: 0.9581
- **Durbin-Watson Test:** Tests for autocorrelation in the residuals of the model.
  - DW = 0.3303 (indicating no autocorrelation)
- **Mean Absolute Percentage Error (MAPE):** Measures the accuracy of the forecasting system.
  - MAPE = 2.08497

## Results
The linear regression model demonstrates a high level of accuracy with an adjusted R-squared value of 0.9581. The analysis reveals significant predictors of life expectancy, including GDP per capita, infant mortality rate, and certain lifestyle factors.

## Conclusion
This study successfully identifies key factors influencing life expectancy and highlights the importance of economic and health-related variables. Policymakers can leverage these insights to develop targeted interventions aimed at improving life expectancy, particularly in developing countries. Future work could explore advanced regression techniques and additional datasets to further enhance the predictive power of the model.

## How to Run the Project

### Prerequisites
- Python 3.x
- Required libraries: pandas, numpy, matplotlib, seaborn, sklearn

### Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/life-expectancy-prediction.git

2. Navigate to the project directory:
   ```bash
   cd life-expectancy-prediction

3. Install the required libraries:
   ```bash
   pip install -r requirements.txt

### Acknowledgements
1. World Bank Open Data
2. Our World in Data
