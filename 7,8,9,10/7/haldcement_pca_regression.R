# Set the environment
getwd()
setwd("/home/saisyam/statprojects")

#Load the libraries required.
library(MASS)
library(ggplot2) # plot
library(calibrate) # textxy??
library(car)#vif, durbin-watson test
library(orcutt)  # To fix atuto-correlation issue
library(leaps) #regsubset
library(qpcR)#To calculate PRESS Residual and PRESS Statistic
library(factoextra)## PCA graphs
library(nortest) #### for normality testing - Anderson-Darling etc

# Read the BEST-HALD-CEMENT Data set
############# Description related to dataset.
#The response variable y is the heat evolved in a cement mix. The four explanatory variables are ingredients of the mix, 
#i.e., x1: tricalcium aluminate, x2: tricalcium silicate, 
#x3: tetracalcium alumino ferrite, x4: dicalcium silicate.
haldcement <- read.csv("BEST-Hald Cement data.csv")
head(haldcement )
str(haldcement )
attach(haldcement)


######################  FIRST LINE 
## Needed to create only when we want to capture all the FINAL plots in a PDF. Should be commented.
#pdf("/home/saisyam/statprojects/HALD_CEMENT/HaldCementProject.pdf")

############################# BUILD initial model ###################
## Most of the functions use linear model created by lm()  as a parameter.
#Ex: influence points,residuals,fitted values,Breusch Pagan test (Homoscadasticity)
# durbinWatsonTest ( Auto correlation) etc

# So let us build an initial model with Y vs X1,X2,X3 and X4
HC_LM_1 <- lm(haldcement$Y ~ ., data = haldcement )

# What's the model - linear equation
HC_LM_1

#Summary of the model ( R2 adjusted etc)
summary(HC_LM_1)
# Result : 	Adjusted R-squared:  0.9736 - very high


############### Chapter 4 : Model Adequacy Checking
#1. The relationship between the study variable and explanatory variables is linear, atleast approximately.
#2. The error term has zero mean.
#3. The error term has constant variance.
#4. The errors are uncorrelated.
#5. The errors are normally distributed.


############### 1. Checking of LINEAR relationship between study and explanatory variables
### Use scatter plot ( Upper triangle) clubbed with correlation matrix (Lower Triangle)
library(ggplot2)
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(haldcement$Y ~ ., data = haldcement,lower.panel = panel.cor)
#### Only when we need to create pdf
#dev.copy(pdf,'scatterplot_corrcoefficients.pdf')
#dev.off()

###  Observations - Correlation
### Y has high correlation with X4 and then with X3. and negligible correlation with X1 and X2.
### X1 has very high correlation with X3 and negligible correlation with others.
### X2 has very high correlation with X4 and negligible correlation with others.
### X3 has no correlation with X4.

###  Observations - (L)inearity
###  Very few data points - 14 only
###  Y vs X1 - pattern not clear, but points spread all over
### Y vs X2  - Seems to be Linear
### Y vs X3  -  pattern not clear, but points spread all over
### Y vs X4  -  tending towards linear
#### X1 and X3 - Pattern not clear, but tending towards linear
#### X2 and X4  - Pretty much linear

########################################### 2. Checking for Influence Points #########################################
inflm.HC_LM_1 <-  influence.measures(HC_LM_1)
which(apply(inflm.HC_LM_1$is.inf, 1, any))
# which observations 'are' influential
dim(summary(inflm.HC_LM_1)) # only these

###  Observations 
#### dimensions of influence measure matrix : n=4 k=9
#### Influence points - Evaluation criteria, will use 3 methods.
#### A point is termed as an influence point and needs further observation, 
## if COOK's D-statistic(D) > 1 
## if DFBETAS > 2/sqrt(n) = 2/sqrt(4)) = 1, i.e DFBETAS > 1
### if DFFITS > 2 * sqrt(k/n)  = 2*sqrt(9/4)= 3 , i.e DFFITS > 3
#### Observation: Point 3 ( DFBETAS). None as per DFFITS and cook's distance D.

#### Further investigation of Point 3
#> haldcement[3,]
#       Y   X1  X2  X3  X4
# 3  104.3  11  56  8   20

# Find 3 sigma limits of X1
mean(haldcement$X1) - 3 * sd(haldcement$X1); mean(haldcement$X1) + 3 * sd(haldcement$X1);
# Find 3 sigma limits of X2
mean(haldcement$X2) - 3 * sd(haldcement$X2);mean(haldcement$X2) + 3 * sd(haldcement$X2)
# Find 3 sigma limits for X3
mean(haldcement$X3) - 3 * sd(haldcement$X2);mean(haldcement$X3) + 3 * sd(haldcement$X3)
# Find 3 sigma limits for X4
mean(haldcement$X4) - 3 * sd(haldcement$X4);mean(haldcement$X4) + 3 * sd(haldcement$X4)

# Find 2 sigma limits of X1
mean(haldcement$X1) - 2 * sd(haldcement$X1); mean(haldcement$X1) + 2 * sd(haldcement$X1);
# Find 2 sigma limits of X2
mean(haldcement$X2) - 2 * sd(haldcement$X2);mean(haldcement$X2) + 2 * sd(haldcement$X2)
# Find 2 sigma limits for X3
mean(haldcement$X3) - 2 * sd(haldcement$X2);mean(haldcement$X3) + 2 * sd(haldcement$X3)
# Find 2 sigma limits for X4
mean(haldcement$X4) - 2 * sd(haldcement$X4);mean(haldcement$X4) + 2 * sd(haldcement$X4)

###   CONCLUSION : Point 3 is within 2 sigma limits of X1,X2,X3 and X4. So retain it.

##########################################3. MULTICOLLINEARITY Check ###############

########### Display VIF
vif(HC_LM_1)
# X1        X2        X3        X4 
# 38.49621 254.42317  46.86839 282.51286 

#CONCLUSION : SEVERE MULTICOLLINEARITY as VIF > 10 for all variables

################################### 3. Checking for normality of Residuals #######################################
ors <- rstandard(HC_LM_1)
# normal probability plot is also known as QQ plot in R ? Seems to be based on quantiles and median.. double check
qqnorm(ors, pch = 1, frame = FALSE)
qqline(ors, col = "steelblue", lwd = 2)
#### Only when we need to create pdf
#dev.copy(pdf,'HC_LM_1_QQplot.pdf')
#dev.off()

### Observation : From graph Seems residuals pretty much follow normal

### Do the normality  significance test
#Shapiro-Wilk’s method is widely recommended for normality test and it provides better power than K-S. 
#It is based on the correlation between the data and the corresponding normal scores.
# H0 : Part of Normal distribution
#H1 : Not part of normal distribution
shapiro.test(rstandard(HC_LM_1))
#### Result
#Shapiro-Wilk normality test
#data:  rstandard(HC_LM_1)
#W = 0.97061, p-value = 0.9014
#CONCLUSION : p-value high, NULL flies. So Residuals follow NORMAL DISTRIBUTION

####### We can also use Anderson-Darling test
ad.test(rstandard(HC_LM_1))

################################# LINEAR Relation between Y and X's ####################
par(mfrow=c(2,2))
plot(haldcement$X1,rstudent(HC_LM_1),xlab = "X1 ",ylab = "rstudent(ti)")
plot(haldcement$X2,rstudent(HC_LM_1),xlab = "X2 ",ylab = "rstudent(ti)")
plot(haldcement$X3,rstudent(HC_LM_1),xlab = "X3 ",ylab = "rstudent(ti)")
plot(haldcement$X4,rstudent(HC_LM_1),xlab = "X4 ",ylab = "rstudent(ti)")

############################## REMEDY FOR MULTICOLLINEARITY ######################
# We can also have pearson correlation coeffcients along with p-values.
library("Hmisc")
haldcement_cormat <- rcorr(as.matrix(haldcement))
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(pmat)  # ut <- upper.tri(cormat) 
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
# Actual Output
flattenCorrMatrix(haldcement_cormat$r, haldcement_cormat$P)

### For correlation, H0: the null hypothesis is that r = 0 -- that there is no relationship between the variables.
##### H1 : r not euql to zero
###CONCLUSION: 
### No Corr : Accept H0 : X1,X2 (0.04); X2,X3 (0.06),X1,X4(0.04),X3,X4 (0.09) 
### Corr exists : Accept H1 : X1,X3 (< 0.05);X2,X4 (< 0.05)
####Question : Which Variables to be selected  for the model ?


############################################## PCA Regression  ######################
##################################  PCA
haldcement_x <- haldcement[,c(-1)]

#Scale the data
#res.pca <- prcomp(haldcement_x, center = FALSE, scale = TRUE)
res.pca <- prcomp(haldcement_x, retx=TRUE,center = FALSE, scale = TRUE)
summary(res.pca)
res.pca$x
library(factoextra)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
##graphics.off() ### IMP: to ensure graphics is cleared

##########  Let us build a model with Transformed PCA data.
# create a new data frame
haldcement_pca <- as.data.frame(cbind(haldcement$Y,res.pca$x))
colnames(haldcement_pca) <- c("Y", "PCA1","PCA2","PCA3","PCA4")
haldcement_pca
class(haldcement_pca)
model_pca1 = lm(Y~PCA1+PCA2,data=haldcement_pca)
model_pca1
PRESS(model_pca1)
vif(model_pca1)
summary(model_pca1)$adj.r.squared


########Recommendation: PCA1: X2 and X4 ( X4 and X3 very close) PCA 2 : X1 and X3


################################ FINAL VARIABLE/MODEL selection ##########
############## No clear Winner, as different model gave different combinations. Use PRESS() to break the tie.
### Model 1 - X1 + X2 - stepwise regression
HC_LM_Model1 = lm(Y~X1+X2)
PRESS(HC_LM_Model1)
vif(HC_LM_Model1)
summary(HC_LM_Model1)$adj.r.squared
#### Model 2 - X2 and X4  - PCA 1
HC_LM_X2_X4 <- lm(Y~X2+X4)
PRESS(HC_LM_X2_X4)
vif(HC_LM_X2_X4)
summary(HC_LM_X2_X4)$adj.r.squared
#######  Model 3 - X4 + X1
HC_LM_X4_X1 <- lm(Y~ X4+X1 )
PRESS(HC_LM_X4_X1)
vif(HC_LM_X4_X1)
summary(HC_LM_X4_X1)$adj.r.squared
######## Model 4 - X2 and X3 ( PCA1 - slightly modified as X2 and X4 are correlated)
HC_LM_X2_X3 <- lm(Y~X2+X3)
PRESS(HC_LM_X2_X3)
vif(HC_LM_X2_X3)
summary(HC_LM_X2_X3)$adj.r.squared

######### CONCLUSION
####### Model1 - X1+X2 : PRESS stat : 93.88255, VIF =1, Radj - 97% , R-squared (predicted) - 0.9654305
####### Model2 -X2+X4- PCA 1 : PRESS stat : 1461.814, VIF > 18, Radj - 61%, R-squared (predicted) - 0.4617298
###### Model 3 - X4+X1  : PRESS stat : 121.2244, VIF =1 , Radj - 96% , R-squared (predicted) - 0.9553627
###### Model 4 - X2+X3 : PRESS stat :701.7432 , VIF =1, Radj - 81%, R-squared (predicted) - 0.7416037
#### Since PRESS stat should be small ( so that R-sq(pred) is high), VIF=1. We will go with Model 1 : X1+X2
################################# FINAL MODEL #################
Model_LM_final = lm(haldcement$Y ~ X1+X2, data = haldcement )
summary(Model_LM_final)
####################### 1. N - Normality Significance test for Residuals.
# H0 : Part of Normal distribution
# H1 : Not part of normal distribution
shapiro.test(rstandard(Model_LM_final))
#####Result 
#Shapiro-Wilk normality test
#data:  rstandard(Model_LM_final)
#W = 0.90219, p-value = 0.1433
####### We can also use Anderson-Darling test
#H0: The data follows the normal distribution
#H1: The data do not follow the normal distribution
ad.test(rstandard(Model_LM_final))
####Result
#Anderson-Darling normality test
#data:  rstandard(Model_LM_final)
#A = 0.61361, p-value = 0.08628
#########Normal Probability plot
ors <- rstandard(Model_LM_final)
# normal probability plot is also known as QQ plot in R ? Seems to be based on quantiles and median.. double check
qqnorm(ors, pch = 1, frame = FALSE)
qqline(ors, col = "steelblue", lwd = 2)
#### RESDUALS FOLLOW NORMAL DISTRIBUTION.
####################### 2. (E) -  HOMOSCADASTICITY TEST
### NULL hypothesis H 0 : σ 1 2 = σ 2 2 = ... = σ n 2
bptest(Model_LM_final)
######Results
#studentized Breusch-Pagan test
#data:  Model_LM_final
#BP = 0.16688, df = 2, p-value = 0.9199
#### RESDUALS have EQUAL VARIANCE .. HOMOSCADASTICTY OK.
####################### 3.  L - Linearity test - e vs X1, e vs X2
par(mfrow=c(2,2))
##### e vs X1
plot(haldcement$X1,rstudent(Model_LM_final),xlab = "X1",ylab = "rstudent(ti)")
##### e vs X2
plot(haldcement$X2,rstudent(Model_LM_final),xlab = "X2",ylab = "rstudent(ti)")
######### OBSERVATION : Seems to be in a band.
####################### 4.  (I) - Independence/AUTO CORRELATION TEST
#The Durbin-Watson (D-W) test is used for testing the hypothesis of lack of first order autocorrelation in the
#disturbance term. The null hypothesis is
# H0 : ρ = 0
#positive autocorrelation of e t ’s ⇒ d < 2
#negative autocorrelation of e t ’s ⇒ d > 2
#zero autocorrelation of e t ’s ⇒ d ≈ 2
durbinWatsonTest(Model_LM_final)
#######RESULT
#lag Autocorrelation D-W Statistic p-value
#1     -0.05450402       1.92164   0.892
#Alternative hypothesis: rho != 0
######## p is high so NULL flies . INDEPENDENCE TEST PASSES. No AUTOCORRELATION issue.
####################### FINAL MODEL  ###############
####### Print the current Model
Model_LM_final
#Call:
#  lm(formula = haldcement$Y ~ X1 + X2, data = haldcement)
#Coefficients:
#  (Intercept)           X1           X2  
#       52.5773       1.4683       0.6623  
# Y = 52.5773  + (1.4683) X1 + ( 0.6623) X2

######################### T-test : between X variables 
######## Do we need both : X2 and X4, X1 and X3
myres1 <- t.test(X2,X4)
myres1$statistic
myres1$p.value
myres2 <- t.test(X1,X3)
myres2$statistic
myres2$p.value
myres3 <- t.test (X1, I(X1^(0.2)))
myres3$p.value
######################  LAST LINE 
#### Need to have this statement to stop copying into PDF
#dev.off()


####################### Checking the need for transformation ########
## Box -Tidwell hypothesis
#H0 : no transformation needed against H1 : transformation needed
#i.e. it is equivalent to testing the null hypothesis that no transformation of
#the explanatory variable is required to linearize the relationship between the
#response variable and the explanatory variable against the alternative that
#some transformation is required
boxTidwell(Y, X1,verbose=TRUE)
#####result
#  MLE of lambda Score Statistic (z) Pr(>|z|)
#      0.23145             -0.7366   0.4614
#iterations =  10 
boxTidwell(Y, X2)
### Result
#MLE of lambda Score Statistic (z) Pr(>|z|)
#-0.71695               -1.35    0.177
#
#iterations =  8 
############ CONCLUSION : P is high (>0.05) so NULL flies. So no transformation needed for X1 and X2.







##########################################  APPENDIX ##################################
## RESULT  : First lm model : Y vs X1,x2,X3 and X4
#Call:
#  lm(formula = haldcement$Y ~ ., data = haldcement)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.1750 -1.6709  0.2508  1.3783  3.9254 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  62.4054    70.0710   0.891   0.3991  
#X1            1.5511     0.7448   2.083   0.0708 .
#X2            0.5102     0.7238   0.705   0.5009  
#X3            0.1019     0.7547   0.135   0.8959  
#X4           -0.1441     0.7091  -0.203   0.8441  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.446 on 8 degrees of freedom
#Multiple R-squared:  0.9824,	Adjusted R-squared:  0.9736 
#F-statistic: 111.5 on 4 and 8 DF,  p-value: 4.756e-07

###########Influence points
#> dim(summary(inflm.HC_LM_1)) # only these
#Potentially influential observations of
#lm(formula = haldcement$Y ~ ., data = haldcement) :
  
#  dfb.1_  dfb.X1  dfb.X2  dfb.X3  dfb.X4  dffit cov.r   cook.d hat  
#1   0.00    0.00    0.00    0.00    0.00    0.00  4.34_*  0.00   0.55
#3  -1.10_*  1.03_*  1.08_*  1.08_*  1.10_* -1.24  2.19    0.30   0.58
#5  -0.02    0.00    0.02    0.00    0.02    0.09  3.00_*  0.00   0.36
#10 -0.05    0.12    0.04    0.08    0.05    0.30  6.33_*  0.02   0.70
#[1] 4 9


######Analysis of influence points
####### Display Point 3 first
#> haldcement[3,]
#   Y    X1 X2 X3 X4
#3 104.3 11 56  8 20
#> # Find 3 sigma limits of X1
#  > mean(haldcement$X1) - 3 * sd(haldcement$X1); mean(haldcement$X1) + 3 * sd(haldcement$X1);
#[1] -10.18564
#[1] 25.10872
#> # Find 3 sigma limits of X2
#  > mean(haldcement$X2) - 3 * sd(haldcement$X2);mean(haldcement$X2) + 3 * sd(haldcement$X2)
#[1] 1.471202
#[1] 94.83649
#> # Find 3 sigma limits for X3
#  > mean(haldcement$X3) - 3 * sd(haldcement$X2);mean(haldcement$X3) + 3 * sd(haldcement$X3)
#[1] -34.91341
#[1] 30.98461
#> # Find 3 sigma limits for X4
#  > mean(haldcement$X4) - 3 * sd(haldcement$X4);mean(haldcement$X4) + 3 * sd(haldcement$X4)
#[1] -20.21454
#[1] 80.21454
#> # Find 2 sigma limits of X1
#  > mean(haldcement$X1) - 2 * sd(haldcement$X1); mean(haldcement$X1) + 2 * sd(haldcement$X1);
#[1] -4.30325
#[1] 19.22633
#> # Find 2 sigma limits of X2
#  > mean(haldcement$X2) - 2 * sd(haldcement$X2);mean(haldcement$X2) + 2 * sd(haldcement$X2)
#[1] 17.03208
#[1] 79.27561
#> # Find 2 sigma limits for X3
#  > mean(haldcement$X3) - 2 * sd(haldcement$X2);mean(haldcement$X3) + 2 * sd(haldcement$X3)
#[1] -19.35253
#[1] 24.57948
#> # Find 2 sigma limits for X4
#  > mean(haldcement$X4) - 2 * sd(haldcement$X4);mean(haldcement$X4) + 2 * sd(haldcement$X4)
#[1] -3.47636
#[1] 63.47636
######################### MULTICOLLINEARITY INVESTIGATION ############
#### Pearson Correlation co-efficients with P-values.
#> flattenCorrMatrix(haldcement_cormat$r, haldcement_cormat$P)
#row column        cor            p
#1    Y     X1  0.7307175 4.552045e-03
#2    Y     X2  0.8162526 6.648249e-04
#3   X1     X2  0.2285795 4.525662e-01
#4    Y     X3 -0.5346707 5.976232e-02
#5   X1     X3 -0.8241338 5.308752e-04
#6   X2     X3 -0.1392424 6.500589e-01
#7    Y     X4 -0.8213050 5.762318e-04
#8   X1     X4 -0.2454451 4.189332e-01
#9   X2     X4 -0.9729550 2.404645e-08
#10  X3     X4  0.0295370 9.236906e-01

#> ######## What is the contribution levels of all variables ############
#> anova(HC_LM_1)#Extra sum of squares
#Analysis of Variance Table
#Response: haldcement$Y
#            Df  Sum Sq Mean Sq  F value    Pr(>F)    
#  X1         1 1450.08 1450.08 242.3679 2.888e-07 ***
#  X2         1 1207.78 1207.78 201.8705 5.863e-07 ***
#  X3         1    9.79    9.79   1.6370    0.2366    
#  X4         1    0.25    0.25   0.0413    0.8441    
#Residuals  8   47.86    5.98                       
#---

#################### Forward selection
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> > Modelsummary=cbind(summary(FSR)$which,R2=summary(FSR)$rsq,SSres=summary(FSR)$rss,AdjR2=summary(FSR)$adjr2,Cp=summary(FSR)$cp,BIC=summary(FSR)$bic)
#> Modelsummary
#  (Intercept) X1 X2 X3 X4        R2     SSres     AdjR2         Cp        BIC
#1           1  0  0  0  1 0.6745420 883.86692 0.6449549 138.730833  -9.462884
#2           1  1  0  0  1 0.9724710  74.76211 0.9669653   5.495851 -39.007873
#3           1  1  1  0  1 0.9823355  47.97273 0.9764473   3.018233 -42.210745
#4           1  1  1  1  1 0.9823756  47.86364 0.9735634   5.000000 -39.675391

###################### Backward Elimination
#> Modelsummary
#  (Intercept) X1 X2 X3 X4        R2     SSres     AdjR2         Cp        BIC
#1           1  0  0  0  1 0.6745420 883.86692 0.6449549 138.730833  -9.462884
#2           1  1  0  0  1 0.9724710  74.76211 0.9669653   5.495851 -39.007873
#3           1  1  1  0  1 0.9823355  47.97273 0.9764473   3.018233 -42.210745
#4           1  1  1  1  1 0.9823756  47.86364 0.9735634   5.000000 -39.675391
#>
#***************************
#> #Stepwise Regression
#  > SWR=regsubsets(Y~.,data=haldcement,method="seqrep")
#> Modelsummary=cbind(summary(SWR)$which,R2=summary(SWR)$rsq,SSres=summary(SWR)$rss,AdjR2=summary(SWR)$adjr2,Cp=summary(SWR)$cp,BIC=summary(SWR)$bic)
#> Modelsummary
#   (Intercept) X1 X2 X3 X4        R2     SSres     AdjR2         Cp        BIC
#1           1  0  0  0  1 0.6745420 883.86692 0.6449549 138.730833  -9.462884
#2           1  1  1  0  0 0.9786784  57.90448 0.9744140   2.678242 -42.329587
#3           1  1  1  1  0 0.9822847  48.11061 0.9763796   3.041280 -42.173433
#4           1  1  1  1  1 0.9823756  47.86364 0.9735634   5.000000 -39.675391
#> ###################
#> ################################ FINAL VARIABLE/MODEL selection ##########
#> ############## No clear Winner, as different model gave different combinations. Use PRESS() to break the tie.
#  > ### Model 1 - X1 + X2 - stepwise regression
#  > HC_LM_Model1 = lm(Y~X1+X2)
#> PRESS(HC_LM_Model1)
#.........10...
#$stat
#3[1] 93.88255
#$residuals
#[1] -2.102014  1.421300 -1.719149 -2.188673 -1.519575  4.574082 -2.040182 -2.735109  2.222715
#[10]  3.027807  4.000507  1.073958 -3.682147
#$P.square
#[1] 0.9654305
#> vif(HC_LM_Model1)
#X1       X2 
#1.055129 1.055129 
#3> summary(HC_LM_Model1)$adj.r.squared
#[1] 0.974414
#> #### Model 2 - X2 and X4  - PCA 1
#  > HC_LM_X2_X4 <- lm(Y~X2+X4)
#> PRESS(HC_LM_X2_X4)
#.........10...
#$stat
#[1] 1461.814
#$residuals
#[1]   7.415438  -6.595926   2.113090   6.462380   1.014148   8.850896 -14.517918 -15.233982
#[9]  -8.744275  24.639258  -9.241352   5.060362  -0.561662
#$P.square
#[1] 0.4617298
#> vif(HC_LM_X2_X4)
#X2       X4 
#18.74113 18.74113 
#> summary(HC_LM_X2_X4)$adj.r.squared
#[1] 0.6160725
#> #######  Model 4 - X4 + X1
#  > HC_LM_X4_X1 <- lm(Y~ X4+X1 )
#> PRESS(HC_LM_X4_X1)
#.........10...
#$stat
#[1] 121.2244
#$residuals
#[1]  3.3553101  2.3262531 -2.6947381 -3.2100180  3.2417664  4.2683983 -1.6088219 -6.3263672
#[9]  0.7802137 -3.1206811  0.1666782  2.1191783 -0.8851098
#$P.square
#[1] 0.9553627
#> vif(HC_LM_X4_X1)
#X4       X1 
#1.064105 1.064105 
#> summary(HC_LM_X4_X1)$adj.r.squared
#[1] 0.9669653
#> ######## Model 3 - X2 and X3 ( PCA1 - slightly modified as X2 and X4 are correlated)
#  > HC_LM_X2_X3 <- lm(Y~X2+X3)
#> PRESS(HC_LM_X2_X3)
#.........10...
#$stat
#[1] 701.7432
#$residuals
#[1] -10.03615246  -4.89899613  -0.75312039   1.18860410  -9.54508177   6.67714264  -6.34731641
#[8]  -0.09523627  -0.38383791  16.95442993   8.58051387   2.51879269  -5.61059875
#$P.square
#[1] 0.7416037
#
#> vif(HC_LM_X2_X3)
#X2       X3 
#1.019772 1.019772 
#> summary(HC_LM_X2_X3)$adj.r.squared
#[1] 0.8164305

#####################  T-test for Feature Selection ######
#$X1

#Welch Two Sample t-test
#data:  x and Y
#t = -19.634, df = 15.586, p-value = 2.062e-12
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -97.47929 -78.44379
#sample estimates:
#  mean of x mean of y 
#7.461538 95.423077 


#$X2

#Welch Two Sample t-test

#data:  x and Y
#t = -7.8744, df = 23.973, p-value = 4.205e-08
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -59.65938 -34.87908
#sample estimates:
#  mean of x mean of y 
#48.15385  95.42308 


#$X3

#Welch Two Sample t-test
#
#data:  x and Y
#t = -18.447, df = 16.212, p-value = 2.625e-12
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -93.25701 -74.05068
#sample estimates:
#  mean of x mean of y 
#11.76923  95.42308 


#$X4

#Welch Two Sample t-test

#data:  x and Y
#t = -10.481, df = 23.732, p-value = 2.207e-10
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -78.31322 -52.53293
#sample estimates:
#  mean of x mean of y 
#30.00000  95.42308 
