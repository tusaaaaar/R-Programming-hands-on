# MLR-Delivery Time
getwd()
setwd("/home/saisyam/statprojects/")
dataset <- read.csv("MLR-Delivery Time.csv")
head(dataset)
str(dataset)

#
library(RWeka)  # Need for modelLR()
#
library(ggplot2)  # for Plot()

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[iris$Species])
}

# First Thing is to do plot Scatterplot Matrix.
# We need 2 new fucntions panel.cor() - correlation panel 
# Adds correlations on the lower panels: The size of the text is proportional to the correlations.
pairs(DeliveryTime ~ ., data = dataset,lower.panel = panel.cor,
      upper.panel = upper.panel)

#The function pairs.panels [in psych package] can be also used to create a scatter plot of matrices, 
#with bivariate scatter plots below the diagonal, histograms on the diagonal, and the Pearson correlation above the diagonal.
library(psych)
pairs.panels(iris[,-5], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# training
modelLR <- LinearRegression(DeliveryTime ~ ., data = dataset)
# You can see the model and contribution of factors to the prediction
modelLR

# Create Design Matrix , i.e add first column of 1's
dm_x <- model.matrix(~Cases+Distance,data=dataset)
class(dm_x)
dim(dm_x)
dm_x
y = matrix(dataset$DeliveryTime)
dim(y)
#b = solve(crossprod(dm_x %*% crossprod(dm_x,y)))
#dim(b)
 
# Hat Matrix by Hand - the below formula is right
#H_m <- X %*% solve(t(X) %*% X) %*% t(X)
H_m <- dm_x %*% solve(t(dm_x) %*% dm_x) %*% t(dm_x)
class(H_m)
diag(H_m)
sum(diag(H_m))


# We will also use the function used by Ramesh Sir
attach(dataset)
MLR=lm(DeliveryTime~Cases+Distance)
#Hat matrix -Diagonal elements
hii=hatvalues(MLR)
hii
sum(hii)