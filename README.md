# Walmart-Sales-Forecasting
This data analysis project aims to provide insights into Walmart's sales patterns, identify factors influencing demand, and build a statistical model to improve demand forecasting. The analysis includes store-specific details, growth rates, holiday impacts, and the creation of a predictive model for better sales forecasting.
# Walmart Sales Data Analysis Project

## Overview

This data analysis project addresses the stock management challenge faced by Walmart due to unforeseen demands. The existing machine learning algorithm lacks accuracy in demand predictions. The project aims to enhance prediction accuracy by incorporating various factors, including economic conditions such as CPI, Unemployment Index, and more.

## Objectives

- Provide insights into Walmart's sales patterns.
- Identify factors influencing demand.
- Build a statistical model for improved demand forecasting.

## Tools and Technologies

- Programming Language: R
- IDE: R Studio
- Libraries Used: `dplyr`, `ggplot2`, `tidyr`, `lubridate`, `car`, and others (specified in R script)
- Machine Learning Algorithm: Linear Regression, Random Forest
- Analysis Techniques: Exploratory Data Analysis (EDA), ETL (Extract, Transform, Load)

## Project Structure

- `data_analysis_script.R`: R script containing the entire data analysis process.
- `WALMART_SALES_DATA.csv`: Dataset used for analysis.

## Key Analysis Steps

1. Exploratory Data Analysis (EDA):
   - Understand data dimensions, structure, and summary statistics.
   - Identify unique values and check for missing or duplicate data.

2. Analysis Tasks:
   - Identify the store with maximum sales.
   - Determine the store with the maximum standard deviation in sales.
   - Analyze quarterly growth rates for Q3’2012.
   - Identify holidays impacting sales positively.

3. Statistical Model:
   - Build a Linear Regression model for Store 1.
   - Evaluate variables like date, CPI, unemployment, and fuel price.
   - Choose the model with the best accuracy.

## Results and Visualizations

- Graphs and charts visualizing sales patterns, store-specific insights, and growth rates.
- Statistical insights derived from the analysis tasks.

## How to Run

1. Open the repository:Walmart-Sales-Forecasting
2. Open the R script in R Studio.
3. Run the script to perform the analysis.

Feel free to explore, contribute, and provide feedback. Happy analyzing!
