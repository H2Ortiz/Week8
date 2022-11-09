#HW8 - Maximum Likelihood
#Tyler Butts, David Ortiz, and Danny Szydlowski

#BLANK CANVAS

library(datasets)
library(tidyverse)
library(dplyr)
library(readr)
library(ggfortify)
library(ggplot2)

# simulate some data

x = rnorm(mean = 15, sd = 3, n = 100)
beta_0 = 5
beta_1 = 0.8

y = beta_0 + beta_1*x + rnorm(mean = 0, sd = 2, n = 100)

plot(y~x)

lin_model = lm(y~x)
summary(lin_model)

autoplot(lin_model)

plot(residuals(lin_model)~fitted(lin_model))

### Estimate using the normal equation

beta = solve(t(x)%*%x) %*% (t(x)%*%y)

### Question 3 ####
# will return in the afternoon

# make a function to calculate rss for each of the values in the grid
rssCalc <- function(y, predValue){sum((predValue-y)^2)}

# setting grid search around a slope of 0.8
b1_slope <- seq(0, 1.6, 0.1)
b0_intercept <- seq(0, 10, 0.1)

gridMatrix = matrix(data = NA, nrow = length(b0_intercept), ncol = length(b1_slope),
                    dimnames = list(b0_intercept, b1_slope))

for(i in 1:nrow(gridMatrix)){
    b0Temp = as.numeric(rownames(gridMatrix)[i])
    print(b0Temp)
    for(j in 1:ncol(gridMatrix)){
        b1Temp = as.numeric(colnames(gridMatrix)[j])
        
        pred_value = b0Temp + b1Temp*x
         gridMatrix[i, j] = rssCalc(y, pred_value)
        
        print(paste(b0Temp, b1Temp))
    }
}


# find the minimum value in the grid we calculated



