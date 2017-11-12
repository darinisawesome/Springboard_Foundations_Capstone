xbar <- 38.5 # sample mean
sigma <- 10 # known population standard deviation
n <- 343 # sample size
mu0 <- 35
print(mu0) # CEO's claimed population mean

z <- (xbar - mu0)/(sigma/sqrt(n))
print(z)

pnorm(z, lower.tail=FALSE) # base R

# Value must be greater than 0.05