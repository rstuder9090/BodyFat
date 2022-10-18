# Body Fat
Project for UW-Madison STAT 629 Module 2 - Group 11. 

Looking for a simple, robust, accurate "rule of thumb" to estimate body fat percentage among men. 

Created with R 4.1.1

## Summary
Find a .pdf file of an Executive Summary for this project.

## Data Folder
Contains any data raw or cleaned
- [BodyFat.csv](Data/BodyFat.csv) raw data
- [BodyFat2.csv](Data/BodyFat2.csv) cleaned data

## Code Folder
Contains any R code for cleaning, analysis, visualization, or Shiny App creation.
- [Data_Cleaning.R](Code/Data_Cleaning.R) code to load the data, create new metric variables for height and weight, and look for outliers.
- [LM.R](Code/LM.R) code to run stepwise regression for linear models and diagnostic plots.
- [app.R](Code/app.R) code to run Shiny App.
- [glmModel.R](Code/glmModel.R)
- [pcaDraft.R](Code/pcaDraft.R) 

## Images Folder
Contains all important images relative to analysis.


## How to use Code
Download all files in Code folder into directory of choice. 
Set working directory to respective directory. 

Run [Data_Cleaning.R](Code/Data_Cleaning.R) to create clean [BodyFat2.csv](Data/BodyFat2.csv). 

Run [LM.R](Code/LM.R) to view model diagnostics and process with graphs.

Run [app.R](Code/app.R) to deploy the Shiny App locally.
