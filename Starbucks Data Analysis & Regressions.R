library(readr)
library(tidyr)
library(stringr)
library(plyr)
library(dplyr)
library(RVAideMemoire)
library(DescTools)
library(corrplot)

#Setup Working Directory
setwd("/Users/manpreetkaurgurtatta/Library/CloudStorage/Box-Box/Sem 1/Data Stats/Assignments/Assignment 4- Regressions")

#Loading and Reading the Dataset
Starbucks <- read.csv("Starbucks_data-3.csv")
View(Starbucks)
head(Starbucks)

##Checking Dimensions of Dataset
dim(Starbucks)
#122 rows  23 columns

#Checking Variable Names
names(Starbucks)

#Checking Data structure
str(Starbucks)

#Renaming files to simplify data
names(Starbucks) <- c('Timestamp', 'Gender', 'Age', 'Status', 'Income', 'Visits', 'ServiceMode', 'Timespend', 
                      'Nearestlocation', 'Membershipcard', 'Frequentpurchase', 'SpendPurchase', 'QualityRate', 
                      'PriceRate', 'PromoRate', 'Ambiance', 'WiFiRate', 'ServiceRate', 'chooseRate',
                       'PromoMethod', 'loyalty', 'BeforePromoSatRate', 'AfterPromoSatRate')

Starbucks_Updated <- Starbucks
str(Starbucks_Updated)
View(Starbucks_Updated)

#Checking for Missing Data
colnames(Starbucks)
sum(is.na(Starbucks))

#If Missing data, use listwise deletion.
Starbucks_Updated <- na.omit(Starbucks)
sum(is.na(Starbucks_Updated))
#No missing data 
View(Starbucks_Updated)

#.....................................................................................................................
#Q1(c): 
#First we will create a new subset containing numeric variables only
names(Starbucks_Updated)
DataViz <- Starbucks_Updated[, c(13,14,15,16,17,18,19,22,23)]
str(DataViz)

#Checking Spearman Correlations
library(corrplot)
corrplot(cor(DataViz, method = "spearman"))
corrplot(cor(DataViz, method = "spearman"), method="number")

#Creating Linear Regression Model 
model1 <- lm(QualityRate ~ ., data=DataViz)
model1


#Checking for VIF
library(DescTools)
VIF(model1)
summary(model1)

#...............................................................................
#Q1(d):
#To create Diagnostic Plots for Model Fit
par(mfrow = c(2, 2))
plot(model_1)
library(ggfortify)
autoplot(model1)

par(mfrow = c(1, 1))

