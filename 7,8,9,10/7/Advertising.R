# Problem Statement
# The Advertising dataset captures sales revenue generated with respect to advertisement spends
# across multiple channels like radio, tv, and newspaper.
# objective:
# Build a linear regression model to:
# • Interpret the coefficients of the model
# • Make predictions
# • Find and analyze model residuals
# • Evaluate model efficiency using RMSE and R-Square values
# https://www.kaggle.com/datasets/ashydv/advertising-dataset

library("tidyverse")
# Import Set Current Working Directory
setwd("E:/Radhesyam/RProjects/mdsc206")
# Check the Directory
getwd()

ad_dataset <-  readr::read_csv("./Advertising.csv")
# What's the data type ? is it  dataframe ? tibble ?
base::class(ad_dataset)

#### DATA EXPLORATION
# How many Rows/Records, How Many Columns/Features
dim(ad_dataset)
# Number of Records
nrow(ad_dataset)
# Number of Features
ncol(ad_dataset)
head(ad_dataset ) # fetch first 6 rows
# what are the column names
base::names(ad_dataset)
#### The first column is just a number, we don't need it
ad_dataset <- dplyr::select(ad_dataset,-1)
base::names(ad_dataset)
head(ad_dataset ) # fetch first 6 rows

####### We generally make our Target Variable as first column
# It will help in Correlation matrix, other plots etc etc
# Last column is 'sales'
#These functions are selection helpers.
#everything() selects all variable. It is also useful in combination with other tidyselect operators.
#last_col() selects the last variable.
ad_dataset <- dplyr::select(ad_dataset,sales, everything())
head(ad_dataset ) # fetch first 6 rows

# quick lookup head + Datatypes
utils::str(ad_dataset ) #
# What are the data types of Columns ?
readr::spec(ad_dataset) # spec() extracts the full column specification from a tibble created by readr.
# Basic statistics
base::summary(ad_dataset)
# Set the default dataframe in the R environment
attach(ad_dataset) # So that we can directly refer to column names/feature names

# Do we have missing values ?
is.na(ad_dataset)
base::sum(is.na(ad_dataset))

# Remove missing values
#ad_dataset_nona <- na.omit(ad_dataset)
#base::sum(is.na(ad_dataset_nona))
#nrow(ad_dataset_nona)

############### Data Visualization
# ggpairs()
#Scatterplots of each pair of numeric variable are drawn on the left part of the figure.
#Pearson correlation is displayed on the right.
#Variable distribution is available on the diagonal.
library(GGally)
ggpairs(data=ad_dataset, title="Advertising Dataset with ggpairs()",
        upper = list(continuous = wrap("cor", size = 7)))

# Nice visualization of correlations
ggcorr(data=ad_dataset, method = c("everything", "pearson"),
       low = "steelblue", mid = "white", high = "yellow")

# Nice visualization of correlations
ggcorr(data=ad_dataset, method = c("everything", "pearson"),
       geom="circle",
       low = "steelblue", mid = "white", high = "darkred")

# Nice visualization of correlations
ggcorr(data=ad_dataset, method = c("everything", "pearson"),
       geom="circle",
       low = "steelblue", mid = "white", high = "darkred",
       min_size = 2, max_size = 9)

#  Wth Values - label = TRUE
ggcorr(data=ad_dataset, method = c("everything", "pearson"),
       geom="circle",,label=TRUE,
       low = "steelblue", mid = "white", high = "darkred",
       min_size = 2, max_size = 9)

#### Correlation Matrix
res <- cor(ad_dataset)
round(res, 2)
res[,"sales"]

###### Prediction Model
# So let us build an initial model with Y vs X1,X2,X3 and X4
Ad_pred_model <- lm(ad_dataset$sales ~ ., data =ad_dataset )

# What's the model - linear equation
#Ad_pred_model

#Summary of the model ( R2 adjusted etc)
summary(Ad_pred_model)

######## Interpreting Significance Codes in lm Summary
#significance code         p-value
#***                 [0, 0.001]
#**               (0.001, 0.01]
#*                 (0.01, 0.05]
#.                  (0.05, 0.1]
#                      (0.1, 1]

# Note: if alpha=0.05 ( 5%),
# then if P > 0.05, Beta is zero
# then if p < 0.05, Beta is Not Zero

> summary(Ad_pred_model)
#
# Call:
# lm(formula = ad_dataset$sales ~ ., data = ad_dataset)
#
# Residuals:
# Min      1Q  Median      3Q     Max
# -8.8277 -0.8908  0.2418  1.1893  2.8292
#
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)  2.938889   0.311908   9.422   <2e-16 ***
# TV           0.045765   0.001395  32.809   <2e-16 ***
# radio        0.188530   0.008611  21.893   <2e-16 ***
# newspaper   -0.001037   0.005871  -0.177     0.86
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.686 on 196 degrees of freedom
# Multiple R-squared:  0.8972,	Adjusted R-squared:  0.8956
# F-statistic: 570.3 on 3 and 196 DF,  p-value: < 2.2e-16
########
####### Prediction
# Predict: TV = 100, radio = 20,newspaper=10
Sales_pred_A = 2.938889 + (100*0.045765) + (20*0.188)
Sales_pred_A
# 11.27539
predict(Ad_pred_model, data.frame(TV = 100, radio = 20,newspaper=10))
# 11.27558
mean(ad_dataset$sales)
# [1] 14.0225

############## In Practice
library(caret)
set.seed(998)
trainPartitionRows <- createDataPartition(ad_dataset$sales, p = .80, list = FALSE)
nrow(ad_dataset)
nrow(trainPartitionRows)
head(trainPartitionRows)
trainDataset <- ad_dataset[ trainPartitionRows,]
testDataset  <- ad_dataset[-trainPartitionRows,]
head(trainDataset)
head(testDataset)

fitControl <- trainControl(## 10-fold CV
                          method = "cv", # repeatedcv
                          number = 10
                          ## repeated ten times
                          #repeats = 10,
                          )

Ad_model_2 <- train(sales ~ .,
                    data = ad_dataset,
                    method = "lm",
                    trControl = fitControl,
                    ## This last option is actually one
                    ## for gbm() that passes through
                    #verbose = TRUE
                    )

Ad_model_2
summary(Ad_model_2)

predict(Ad_model_2, data.frame(TV = 100, radio = 20,newspaper=10))


##### Tests
# library(MASS)
# shapiro.test(ad_dataset$TV)
# x <- rnorm(1000, mean=90, sd=5)
# shapiro.test(x)