#Question 1
rm(list = ls())

# a
n <- 100
S <- 1000
beta0 <- 1
beta1 <- 2

hat_beta0l <- numeric(S) 
hat_beta1l <- numeric(S)

hat_beta0p <- numeric(S) 
hat_beta1p <- numeric(S)
for (s in 1:S){
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 2)
  y <- as.numeric(beta0 + beta1 * x > u)
  
  hat_beta0l[s]<- coef(glm(y ~ x, family = "binomial"))[1]
  hat_beta1l[s]<- coef(glm(y ~ x, family = "binomial"))[2]
  
  hat_beta0p[s]<- coef(glm(y ~ x,  family = binomial(link = "probit")))[1]
  hat_beta1p[s]<- coef(glm(y ~ x,  family = binomial(link = "probit")))[2]
  
}

# b
mean(hat_beta0l)
mean(hat_beta1l)

mean(hat_beta0p)
mean(hat_beta1p)

# c
hist(hat_beta0l)
hist(hat_beta1l)

hist(hat_beta0p)
hist(hat_beta1p)

# d
# changing U <- rnorm(n, 0, 1) to a different sd you are no longer using the same paramters.
# instead estimating the ratio of parameter/sd so we normalize sigma=1.

#Question 2
rm(list = ls())

# a
n <- 100
S <- 1000
beta0 <- 0
beta1 <- 3
c <- 0
hat_beta1 <- numeric(S)
for (s in 1:S){
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  y <- pmax(rep(c,n), beta0 + beta1 * x + u)
  hat_beta1[s] <- cov(x, y)/var(x)
}

# b
hist(hat_beta1) 


# c
rm(list = ls())
n <- 100
S <- 1000
beta0 <- 0
beta1 <- 3
hat_beta1 <- numeric(S)
bias<- function(c) {
  for (s in 1:S){
    x <- rnorm(n, 0, 1)
    u <- rnorm(n, 0, 1)
    y <- pmax(rep(c,n), beta0 + beta1 * x + u)
    hat_beta1[s] <- cov(x, y)/var(x)
  } 
  mean(hat_beta1) - beta1
}

grid <- seq(-3, 0, 0.1)
biasc <- sapply(grid, bias)
plot(grid, biasc, type = "l")

#Question 3
# a
# when p is 1 the y is more varied then if p is 0.9, p = 0.9 is more centered around 0 and in a tighter range between (-40) ~ 40
# this mean that unit root process has caused problem for statistical inference as it deviates from 0 more often
rm(list = ls())
p <- 1
T <- 300
set.seed(0)
error_terms_ar <- rnorm(T+1, 0, 10) 
y <- 0
for (t in 2:T){
  y[1] = error_terms_ar[1]
  y[t] = p*y[t-1] + error_terms_ar[t]
}
plot.ts(y, type="l")
arima(y , order = c(1, 0, 0))

# p = 0.9
rm(list = ls())
p <- 0.9
T <- 300
set.seed(0)
error_terms_ar <- rnorm(T+1, 0, 10) 
y <- 0
for (t in 2:T){
  y[1] = error_terms_ar[1]
  y[t] = p*y[t-1] + error_terms_ar[t]
}
plot.ts(y, type="l")
arima(y , order = c(1, 0, 0))


# b
rm(list = ls())
p <- 0.9
T <- 300
S <- 1000
set.seed(0)
beta_ols <- numeric(S)
for (i in 1:S){
  error_terms_ar <- rnorm(T+1, 0, 1) 
  y <- numeric(T)
  for (t in 2:T){
    y[1] = error_terms_ar[1]
    y[t] = p * y[t-1] + error_terms_ar[t]
  }
  beta_ols[i] <- arima(y, order = c(1, 0, 0))$coef[1] 
  
}

hist(beta_ols)

# c
rm(list = ls())
p <- 1
T <- 300
S <- 1000
set.seed(0)
beta_ols <- numeric(S)
for (i in 1:S){
  error_terms_ar <- rnorm(T+1, 0, 1)
  y <- numeric(T)
  for (t in 2:T){
    y[1] = error_terms_ar[1]
    y[t] = p * y[t-1] + error_terms_ar[t]
  }
  beta_ols[i] <- arima(y, order = c(1, 0, 0))$coef[1] 
  
}

hist(beta_ols)

# d
# Error in solve.default(res$hessian * n.used, A) : 
#   Lapack routine dgesv: system is exactly singular: U[1,1] = 0
# In addition: Warning message:
# In arima(y, order = c(1, 0, 0)) :
#   possible convergence problem: optim gave code = 1

# assuming this is caused by root unit process as we have p = 1 with arima(y, order = c(1, 0, 0)
# I think normal approximation is best used when we are using it for non unit root values

