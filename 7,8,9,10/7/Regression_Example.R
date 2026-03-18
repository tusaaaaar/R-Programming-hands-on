# title: "Predictive Analytics - Machine Learning"
# 

## Overview
### Machine Learning Process

# 
# 
# ### Regression
# + Linear Regression
# 
# ### Classification
# + Logistic Regression
# 
# ### Model Improvement, Evaluation/Validation
# 
# ## Predictive Analytics
# + Analysing historical data points to make predictions for current data points
# 
# ### Training/Testing/Prediction Phases
# + Trainng Phase ( Optional : Validation) : Learn a model from training data ( Optional: Validating the learning along the way)
# + Testing phase : Test the model for accuracy
# + Prediction phase: Use the trained model to predict the outcome for unknown data points ( Ex: Predict insurance charges, predict cost of a home, predict price of a stock)  
# 
# #### Testing/Tuning The Model  
# 
# ![Testing](D:/Radhesyam/Personal_Material/Sivam_DS/22 Regression/Testing.PNG)

### ML Model Types

# #### Regression
# + Numeric/continuous value prediction (ex: y = mx + c, y is continous data) 
# + Example : Stock Market Price, House Price

# #### Classification
# + Categorical Data predictions/discrete values (ex: y = mx + c, y is discrete data)

# ### ML Model Metrics
# + F-measure for classification model
# + MSE measure (mean_squared_error) for Regression Model

# ## Regression Model
# 
# + A Statistical/Machine learning technique which helps to identify the relationship between two variables : Y - target (dependent) and X - predictor (independent)
# + Height and weight - as height increases, you'd expect the weight to increase, but not perfectly.

# ### Which Line is the Best FIT ?
# 
# ![Regression](D:/Radhesyam/Personal_Material/Sivam_DS/22 Regression/Regression.PNG)

#### Least Sum of Squares 
library(ggplot2)
## Height_Weight Dataset
height <-  c(63L,64L,66L,69L,69L,71L,71L,72L,73L,75L)
weight <-  c(127L,121L,142L,157L,162L,156L,169L,165L,181L,208L)
hw_data <-  data.frame(h=height,w=weight)
# Line 1 : w = -331.2 + 7.1h
# Line 2 : w = -266.5 + 6.1h
Label1 <- "w = -331.2 + 7.1h"
Label2 <- "w = -266.5 + 6.1h"
hwplot <- ggplot(hw_data, aes(x=height, y=weight)) +
geom_point(colour = "red") +
geom_abline(slope=7.1, intercept=-331.2,color="blue",linetype="dashed") +
geom_text(aes(x = 70, y = 180, label = Label1 ) ) +
geom_abline( slope=6.1, intercept=-266.5,color="blue") +
geom_text(aes(x = 68, y = 140, label = Label2 ) )
hwplot


#### Regression Line
hwplot +
geom_smooth(method = "lm",se = FALSE)

## Build the Linear Regression Model
hw_model <- lm(data=hw_data,formula = w ~ h )
hw_model

## Now predict for a new data point
new_height = data.frame(h=c(79))
new_weight = predict(hw_model,newdata=new_height,level = 0.95)
# What is the predicted new weight
new_weight
# Can we calculate from the formula
(-266.534) +(6.138) * 79

## Are the two Values ( calculated using formula and returned by predict()) as expected ? Are they Matching ?
#Why the values are not matching, if they are not same ?

#summary(hw_model)
# Calculate with Estimates of Intercept and Height
#(-266.5344) + (6.1376) * 79
