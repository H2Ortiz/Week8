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



