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

autoplot(lin_model) # Diagnostics 
coef = coef(lin_model)
coef

### Estimate using the normal equation

beta = solve(t(x)%*%x) %*% (t(x)%*%y)
beta # A little higher than our estimate of .788

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
minvalue = which(gridMatrix==min(gridMatrix), arr.ind=T)
min_coefs = c(b0_intercept[minvalue[1]], b1_slope[minvalue[2]]) 
min_coefs


# Best params
min_rss_index <- which(rss_matrix==min(rss_matrix), arr.ind=T)
min_rss_coefs <- c(b0_eval[min_rss_index[1]], b1_eval[min_rss_index[2]])
# Visualize grid search
image(x=b0_eval, y=b1_eval, z=rss_matrix, las=1, xlab="B0", ylab="B1")
points(x=min_rss_coefs[1], y=min_rss_coefs[2], pch=16, cex=1)

## Question 4 ##==================
# Function to calculate the negative log-likelihood
negll_calc <- function(y, predValue, sigma){
    -sum(dnorm(x=y, mean=predValue, sd=sigma, log=T)) # Negative because negative log likelihood
}

# setting grid search around a slope of 0.8 and intercept of 5
b1_slope <- seq(0.001, 1.6, 0.1)
b0_intercept <- seq(0, 10, 0.1)
# add a sigma term here, error around .06 from Q1 model
sigma_value = seq(0.001, 1, 0.01)
nll_array <- array(data=NA, dim=c(length(b0_intercept), length(b1_slope), length(sigma_value)), 
                   dimnames=list(b0_intercept, b1_slope, sigma_value))

# Begin grid search: i <- 1; j <- 1; k <- 1
for(i in 1:length(b0_intercept)){
    b0_curr <- b0_intercept[i]
    for(j in 1:length(b1_slope)){
        b1_curr <- b1_slope[j]
        for(k in 1:length(sigma_value)){
            sigma <- sigma_value[k]
            predValue <- b0_curr + b1_curr*x
            nll <- calc_nll(y, predValue, sigma)
            nll_array[i,j,k] <- nll
        }
    }
}

# Best params
min_nll_index <- which(nll_array==min(nll_array), arr.ind=T)
min_nll_coefs <- c(b0_intercept[min_nll_index[1]], 
                   b1_slope[min_nll_index[2]],
                   sigma_value[min_nll_index[2]])
min_nll_coefs








