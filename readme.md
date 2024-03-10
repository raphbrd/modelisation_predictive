# Forecasting the net load during the soberty period in France (2022-2023)

This is a project made during the first year of the [Mathematics and AI Master at Paris-Saclay University](https://www.universite-paris-saclay.fr/en/education/master/mathematics-and-applications/m1-mathematiques-et-intelligence-artificielle). 

Authors: Arthur Reidenbach et Raphael Bordas

## Dataset

The main dataset contains the French electricity load between 2013 and 2023 in MW (on a daily basis), along various independant variables (weather, lagged production, etc...).

The true net load is known from 2013 until Sept. 1st, 2022. Thus, the test set ranges between Sept. 2nd 2022 and Oct. 1st 2023. All predictions were submitted to a private Kaggle competition.

## Scripts

This repository is organised as follow : 
- a set of ordered scripts preprocess data and run the models (mainly GAM, random forests and ensemble methods). The scripts are meant to be run in the correct order, as some depencies and variables are set in previous files.
- some utility script are located in the corresponding folder.
- the code used for the exploratory analysis is mainly located in the R Markdown report.

## Forecasting

All predictions are evaluated according to the pinball loss for the quantile 0.95. 


