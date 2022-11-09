#HW8 - Maximum Likelihood
#Tyler Butts, David Ortiz, and Danny Szydlowski

#BLANK CANVAS

# simulate some data

x = rnorm(mean = 15, sd = 3, n = 100)
beta_0 = 5
beta_1 = 0.8

y = beta_0 + beta_1*x + rnorm(mean = 0, sd = 2, n = 100)

plot(y~x)

