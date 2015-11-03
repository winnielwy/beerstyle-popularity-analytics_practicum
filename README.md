# beerstyle-popularity-analytics_practicum

In order to guide companies how to reach their target consumers to win business in the competitive brewery industry, my team and I conducted a beer brand popularity prediction analysis based on geo-location related data under the guidance of DELOITTE and our professors.

In the data collection step, I used Untapp (A Beer Rating APP) Application Program Interface(API) to get consumers check in records. I also converted coordinates to GEOID so that I could combine the original dataset with the demographic information.

In the data cleaning step, I converted United universal time to local time using Pytzwhere (a Python library), resampled the imbalanced data, transformed and standardized the dataset and dealt with missing values. 
After preprocessing the data, I stored the dataset in SQL and did some summary statistics. Additionally, I used Tableau to do some descriptive analysis and map applications, and used SAS to do Time Series Forecasting to predict the trend and future demands of different beer brands in US.

In the modeling step, I used the Logistics Regression model in R to predict the likelihood that beer brands will be bought in a GEOID. Other methods included Random Forest and Decision Tree.
After comparing the misclassification rate of the test set and ROC curve of different models, we found that Random Forest was the best model with accuracy around 94%.

The final result shows that locations with a higher unhappy marriage rate may result in the possibility of higher beer drinking. Additionally, different ethnic groups have different beer brand preferences.
