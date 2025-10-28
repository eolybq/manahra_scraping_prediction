# Simulated market stock price prediction

**Project has its own report available (in Czech) locally here:** [As PDF](/report.pdf) [As DOCX](/report.docx)

This project takes data from simulated market game that taked place in my University and shows how linear regression model can perform in predicting stock price of enterprices on this simulated market.
Main aim is to showcase data manipulation, data cleaning, processing and web scraping, in-depth analysis and model evaluation, aswell as prediciton evaluation, data visualization. 
Prediction for specific enterprise is based on financial statements (Balance sheet, Profit & Loss statement) and past values of stock price scraped from Uni simulated game website. 


## Tools used
- R (Automated web scraping, Tidyverse)
- Linear regression
- Model evaluation, prediction, statistical analysis


## Outcomes

**Predictions on test data:**

| Predictions   | Real   |
|--------------:|-------:|
|    -2.7334    |   -28  |
|   103.4340    |   126  |
|     4.8266    |    -9  |
|     5.7218    |     0  |
|     3.3474    |    -5  |
|     4.7154    |    33  |
|    -5.0702    |    -8  |
|     5.4813    |     1  |


**Predictions in plot:**

![Predictions plot](plots_tabs/preds.png)