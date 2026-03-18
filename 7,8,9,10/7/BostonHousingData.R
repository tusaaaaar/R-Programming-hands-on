library("tidyverse")
# Import Set Current Working Directory
setwd("E:/Radhesyam/RProjects/mdsc206")
# Check the Directory
getwd()
## https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html
### https://www.kaggle.com/datasets/altavish/boston-housing-dataset
# Variables
# There are 14 attributes in each case of the dataset. They are:
# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town.
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per $10,000
# PTRATIO - pupil-teacher ratio by town
# B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT - % lower status of the population
# MEDV - Median value of owner-occupied homes in $1000's
# load the Boston housing Dataset
boston_dataset <-  readr::read_csv("./Regression/HousingData.csv")
# What's the data type ? is it  dataframe ? tibble ?
base::class(boston_dataset)

#### DATA EXPLORATION
# How many Rows/Records, How Many Columns/Features
dim(boston_dataset)
# Number of Records
nrow(boston_dataset)
# Number of Features
ncol(boston_dataset)
head(boston_dataset ) # fetch first 6 rows
# what are the column names
base::names(boston_dataset)
# quick lookup head + Datatypes
utils::str(boston_dataset ) #
# What are the data types of Columns ?
readr::spec(boston_dataset) # spec() extracts the full column specification from a tibble created by readr.
# Basic statistics
base::summary(boston_dataset)
attach(boston_dataset) # So that we can directly refer to column names/feature names

# Do we have missing values ?
is.na(boston_dataset)
base::sum(is.na(boston_dataset))

# Remove missing values
boston_dataset_nona <- na.omit(boston_dataset)
base::sum(is.na(boston_dataset_nona))
nrow(boston_dataset_nona)

############### Data Visualization
# ggpairs()
#Scatterplots of each pair of numeric variable are drawn on the left part of the figure.
#Pearson correlation is displayed on the right.
#Variable distribution is available on the diagonal.
library(GGally)
ggpairs(data=boston_dataset_nona, title="Boston Dataset with ggpairs()")
##
# panel.cor <- function(x, y){
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- round(cor(x, y), digits=2)
#   txt <- paste0("R = ", r)
#   cex.cor <- 0.8/strwidth(txt)
#   text(0.5, 0.5, txt, cex = cex.cor * r)
# }
ggpairs(data=boston_dataset_nona, title="Boston Dataset with ggpairs()",
        upper = list(continuous = wrap(cor)))

# Nice visualization of correlations
ggcorr(data=boston_dataset_nona, method = c("everything", "pearson"))

#######
# So let us build an initial model with Y vs X1,X2,X3 and X4
Boston_pred_model <- lm(boston_dataset_nona$MEDV ~ ., data = boston_dataset_nona )

# What's the model - linear equation
#HC_LM_1

#Summary of the model ( R2 adjusted etc)
summary(Boston_pred_model)