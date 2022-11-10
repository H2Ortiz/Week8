#HW8 - Maximum Likelihood
#Tyler Butts, David Ortiz, and Danny Szydlowski

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

# make a function to calculate rss for each of the values in the grid
rssCalc <- function(y, predValue){sum((predValue-y)^2)}

# setting grid search around a slope of 0.8
b1_slope <- seq(0, 1.6, 0.1)
b0_intercept <- seq(0, 10, 0.1)

gridMatrix = matrix(data = NA, nrow = length(b0_intercept), ncol = length(b1_slope),
                    dimnames = list(b0_intercept, b1_slope))
i = 1
j = 1
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
# min_rss_index <- which(gridMatrix==min(gridMatrix), arr.ind=T)
# min_rss_coefs <- c(b0_eval[min_rss_index[1]], b1_eval[min_rss_index[2]])

# Visualize grid search
image(x=b0_intercept, y=b1_slope, z=gridMatrix, las=1, xlab="B0", ylab="B1")
points(x=min_coefs[1], y=min_coefs[2], pch=16, cex=1)

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

# Begin grid search: 
i <- 1
j <- 1
k <- 1
for(i in 1:length(b0_intercept)){
    b0_curr <- b0_intercept[i]
    for(j in 1:length(b1_slope)){
        b1_curr <- b1_slope[j]
        for(k in 1:length(sigma_value)){
            sigma <- sigma_value[k]
            predValue <- b0_curr + b1_curr*x
            nll <- negll_calc(y, predValue, sigma)
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


#Q5 solve using optim -----

#Didnt know there was an answer key attempted to make optim work
#it works but sigma isn't optimized, but b0 and b1 values are!

# data <- as.data.frame(cbind(x,y))
# 
# rssCalc2 <- function(data, par){
#     with(data,sum((par[1]+par[2] *x - y)^2)) 
#         }
# 
# optim_data <- optim(par =c(4.9, 0.8), fn = rssCalc2, data = data)
# 
# optim_data$par
# 
# plot(y ~ x, data = data, main="Least square regression", ylim = c(0,30), xlim = c(0,30))
# abline(a = optim_data$par[1], b = optim_data$par[2], col = "red")


calc_nll <- function(y_obs, y_pred, sigma){
    -sum(dnorm(x=y_obs, mean=y_pred, sd=sigma, log=T))
}


obj_func <- function(par){
    b0 <- par[1]
    b1 <- par[2]
    sigma <- par[3]
    y_pred <- b0 + b1*x
    nll <- calc_nll(y, y_pred, sigma)
}

# Estimate parameters using optim()
optfit <- optim(par=c(0,1,2), fn = obj_func )
optcoefs <- optfit$par

optcoefs

plot(y ~ x, data = data, main="Optim Least square regression", ylim = c(0,30), xlim = c(0,30))
abline(a = optcoefs[1], b = optcoefs[2], col = "red")

#Q6 Plot a likelihood profile for the slope parameter while estimating the conditional MLEs of the intercept and sigma for each plotted value of the slope parameter 

# Nicer function for calculating NLL
calc_nll_new <- function(b0, b1, sigma, y_obs){
    y_pred <- b0 + b1*x
    nll <- calc_nll(y_obs, y_pred, sigma)
}

# Params
b0_hold <- beta_0 * 1.05
sigma_hold <- sigma * 0.95

# Estimate NLL for varying slopes
b1s <- seq(0,2,0.01)
nlls <- sapply(1:length(b1s), function(x) calc_nll_new(b0_hold, b1s[x], sigma_hold, y))
plot(nlls~b1s, type="l", bty="n", las=1, xlab="B1", ylab="NLL")
abline(b = 0, a = min(nlls))
points(x = 0.84, y =  min(nlls), col = "red", pch = 20, cex = 2)


# Q7: Plot the joint likelihood surface for the intercept and slope parameters
# holding sigma constant at its MLE.  Is there evidence of confounding between
# these two parameters (i.e., a ridge rather than a mountain top)?

# Params
b1_eval <- seq(0,3,0.1)
b0_eval <- seq(-1,1,0.1)
sigma_hold <- sigma

# Setup grid search
nll_matrix <- matrix(data=NA, nrow=length(b0_eval), ncol=length(b1_eval), 
                     dimnames=list(b0_eval, b1_eval))
# Begin grid search: i <- 1; j <- 1
for(i in 1:length(b0_eval)){
    b0_curr <- b0_eval[i]
    for(j in 1:length(b1_eval)){
        b1_curr <- b1_eval[j]
        nll <- calc_nll_new(b0_curr, b1_curr, sigma_hold, y)
        nll_matrix[i,j] <- nll
    }
}
# Visualize grid search
image(x=b0_eval, y=b1_eval, z=nll_matrix, las=1, xlab="B0", ylab="B1")
contour(x=b0_eval, y=b1_eval, z=nll_matrix, las=1, xlab="B0", ylab="B1")
persp(x=b0_eval, y=b1_eval, z=nll_matrix, theta=80, phi=30)



