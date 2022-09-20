getwd()

install.packages("ivreg", dependencies = TRUE)
library("ivreg")


# Question 1
# a
# b
rm(list = ls()) 

S <- 1000 
n <- 500

beta0 <- 1
beta1 <- 1
pi0 <- 1 
pi1 <- 1
rho <- 0 

set.seed(0)

ols <- numeric(S)
iv <- numeric(S)

for (i in 1:S){
  z <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  h <- rnorm(n, 0, 1)
  v <- rho * u + (1 - rho) * h
  x <- pi0 + pi1 * z + v
  y <- beta0 + beta1*x + u
  ols_est <- lm(y ~ x)
  ols[i] <- coef(ols_est)["x"]
  iv_est <- ivreg(y ~ x | z)
  iv[i] <- coef(iv_est)["x"]
}

# c
plot(density(ols), xlim = c(0.7, 1.7))  
lines(density(iv), col = "red")

# d 
mean(ols)
mean(iv)
sd(ols)
sd(iv)



# Question 2 
# a
rm(list = ls())

S <- 100
n <- 1000

beta0 <- 0
beta1 <- 1
gamma <- 1

beta1hat_ols <- numeric(S)
avar <- numeric(S)
avar_R <- numeric(S)

for (i in 1:S){
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  y <- beta0 + beta1 * x + (1 + gamma * x) * u
  
  beta1hat_ols[i] <- cov(x,y)/var(x)   
  beta0hat_ols <- mean(y) - beta1hat_ols[i] * mean(x)
  hatv <- y - beta0hat_ols - beta1hat_ols[i] * x 
  varx <- mean((x - mean(x))^2)
  avar[i] <- (1/n) * mean(hatv^2)/varx
  avar_R[i] <- (1/n) * mean((x-mean(x))^2 * hatv^2)/varx^2
}

# b
ci_lower <- beta1hat_ols - 1.96*sqrt(avar)
ci_upper <- beta1hat_ols + 1.96*sqrt(avar)
reject <- as.logical((ci_lower>1) + (ci_upper<1))

ci_lower_R <- beta1hat_ols - 1.96*sqrt(avar_R)
ci_upper_R <- beta1hat_ols + 1.96*sqrt(avar_R)
reject_R <- as.logical((ci_lower_R>1) + (ci_upper_R<1)) 

1-mean(reject)
1-mean(reject_R)

par(mfrow=c(1,2))

color <- rep(gray(.5),100)  
color[reject[1:100]] <- "black"  

plot(0, xlim = c(0.8, 1.2), ylim = c(1, 100), ylab = "Sample #", main = "Assuming Homoskedasticity")
abline(v = 2,lty = 2)

for (i in 1:100){
  lines(c(ci_lower[i], ci_upper[i]), c(i, i), col = color[i], lwd = 2)
}

color <- rep(gray(.5),100)
color[reject_R[1:100]] <- "black"

plot(0, xlim = c(0.8,1.2), ylim = c(1, 100), ylab = "Sample #", main = "Heteroskedasticity Robust")
abline(v = 2,lty = 2)

for (i in 1:100){
  lines(c(ci_lower_R [i],ci_upper_R [i]), c(i, i), col = color[i], lwd=2)
}

# c
rm(list = ls())

S <- 100
n <- 1000

beta0 <- 0
beta1 <- 1
gamma <- 0

beta1hat_ols <- numeric(S)
avar <- numeric(S)
avar_R <- numeric(S)

for (i in 1:S){
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  y <- beta0 + beta1 * x + (1 + gamma * x) * u
  
  beta1hat_ols[i] <- cov(x,y)/var(x)   
  beta0hat_ols <- mean(y) - beta1hat_ols[i] * mean(x)
  hatv <- y - beta0hat_ols - beta1hat_ols[i] * x 
  varx <- mean((x - mean(x))^2)
  avar[i] <- (1/n) * mean(hatv^2)/varx
  avar_R[i] <- (1/n) * mean((x-mean(x))^2 * hatv^2)/varx^2
}

ci_lower <- beta1hat_ols - 1.96*sqrt(avar)
ci_upper <- beta1hat_ols + 1.96*sqrt(avar)
reject <- as.logical((ci_lower>1) + (ci_upper<1))

ci_lower_R <- beta1hat_ols - 1.96*sqrt(avar_R)
ci_upper_R <- beta1hat_ols + 1.96*sqrt(avar_R)
reject_R <- as.logical((ci_lower_R>1) + (ci_upper_R<1)) 

1-mean(reject)
1-mean(reject_R)

par(mfrow=c(1,2))

color <- rep(gray(.5),100)  
color[reject[1:100]] <- "black"  

plot(0, xlim=c(0.8, 1.2), ylim=c(1, 100), ylab="Sample #", main = "Assuming Homoskedasticity with gamma = 0")
abline(v = 1,lty = 2)

for (i in 1:100){
  lines(c(ci_lower[i], ci_upper[i]), c(i, i), col = color[i], lwd = 2)
}

color <- rep(gray(.5),100)
color[reject_R[1:100]] <- "black"

plot(0, xlim = c(0.8,1.2), ylim = c(1, 100), ylab = "Sample #", main = "Heteroskedasticity Robust with gamma = 0")
abline(v = 1, lty = 2)

for (i in 1:100){
  lines(c(ci_lower_R [i],ci_upper_R [i]), c(i, i), col = color[i], lwd=2)
}

# d: we used the estimator of heteroskdesti 
rm(list = ls())

S <- 100 
n <- 1000 

beta0 <- 0
beta1 <- 1

beta1hat_ols <- numeric(S)
avar <- numeric(S)
avar_R <- numeric(S)

emp_coverage <- function(gamma) {
  set.seed(0)
  for (i in 1:S){
    x <- rnorm(n, 0, 1)
    u <- rnorm(n, 0, 1)
    y <- beta0 + beta1 * x + (1 + gamma * x) * u
    
    beta1hat_ols[i] <- cov(x,y)/var(x)   
    beta0hat_ols <- mean(y) - beta1hat_ols[i] * mean(x)
    hatv <- y - beta0hat_ols - beta1hat_ols[i] * x 
    varx <- mean((x - mean(x))^2)
    avar[i] <- (1/n) * mean(hatv^2)/varx
    avar_R[i] <- (1/n) * mean((x-mean(x))^2 * hatv^2)/varx^2
  }
  
  
ci_lower <- beta1hat_ols - 1.96 * sqrt(avar)
ci_upper <- beta1hat_ols + 1.96 * sqrt(avar)
reject <- as.numeric((ci_lower > 1) + (ci_upper < 1)) 
  
ci_lower_R <- beta1hat_ols - 1.96 * sqrt(avar_R)
ci_upper_R <- beta1hat_ols + 1.96 * sqrt(avar_R)
reject_R <- as.numeric((ci_lower_R > 1) + (ci_upper_R < 1)) 
  
cbind(1 - mean(reject), 1 - mean(reject_R) )
}

emp_coverage(0.3)

grid <- seq(from = 0, to = 2, by = 0.1) 
coverage <- sapply(X = grid,  FUN = emp_coverage)

par(mfrow=c(1,2))
plot(grid, coverage[1,])
plot(grid, coverage[2,])


# Question 3
# a: cov(X, Z) = cov(pi0 + pi1Z + V, Z) = cov(pi1Z, Z) = cov(pi1, Z, Z) = pi1 cov(Z, Z) =  pi1 var(Z) = pi1 var(1) = pi1 

# b
rm(list = ls()) 

S <- 1000 
n <- 1000 

beta0 <- 0
beta1 <- 2
pi0 <- 0
pi1 <- 1/sqrt(n)

rho <- 0.9

set.seed(0)

iv <- numeric(S)
beta1hat <- numeric(S)
avar <- numeric(S)

for (i in 1:S){
  z <- rnorm(n,0,1)
  u <- rnorm(n,0,1)
  h <- rnorm(n,0,1)
  v <- rho*u + (1-rho) * h
  x <- pi0 + pi1 * z + v
  y <- beta0 + beta1 * x + u
  
  beta1hat[i] <- cov(z,y)/cov(z,x)
  beta0hat <- mean(y)- beta1hat[i] * mean(x)
  hatu <- y- beta0hat - beta1hat[i] * x
  covxz <- mean((x - mean(x)) * (z - mean(z)))
  avar[i] <- 1/n * mean((z - mean(z))^2 * hatu^2)/covxz^2
  
}

# c
beta_norm <- (beta1hat-beta1)/sqrt(avar)
par(mfrow=c(1, 1))

plot(density(beta_norm))
curve(dnorm(x, 0, 1), add = TRUE, lty = 2)


# d

ci_lower <- beta1hat - 1.96*sqrt(avar)
ci_upper <- beta1hat + 1.96*sqrt(avar)
reject <- as.logical((ci_lower>2) + (ci_upper<2)) 

1-mean(reject)

par(mfrow=c(1, 1))

color <- rep(gray(.5), 100)
color[reject [1:100]] <- "black"

plot(0, xlim = c(0, 4), ylim = c(1 , 100), ylab = "Sample #", main = "Assuming Homoskedasticity")
abline(v = 2,lty = 2)
for (i in 1:100){
  lines(c(ci_lower[i], ci_upper[i]), c(i, i), col = color[i], lwd = 2)
}


# Question 4
# a: Yes the graphs were close to each other and was pretty similar even if it was not the same, this is mostly due to our betas 
rm(list = ls())
set.seed(0)

S <- 1000 
n <- 100

beta <- 0
beta0 <- 0
alpha <- 0

betahat_ols <- numeric(S)
avar_betahat_ols <- numeric(S)

for (i in 1:S){
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  y <- alpha + beta * x + u
  
  ols_est <- lm(y~x)
  betahat_ols[i] <- coef(ols_est)["x"]
  alpha_est <- coef(ols_est)["(Intercept)"]
  uhat <- y - alpha_est - betahat_ols[i] * x
  varx <- mean((x - mean(x))^2)
  avar_betahat_ols[i] <- (1/n) * mean(uhat^2)/varx
  }

ci_lower <- betahat_ols - 1.96 * sqrt(avar_betahat_ols)
ci_upper <- betahat_ols + 1.96 * sqrt(avar_betahat_ols)
reject <- as.logical((ci_lower>1) + (ci_upper<1)) 

1-mean(reject) 

plot_d <- (betahat_ols - beta0)/sqrt(avar_betahat_ols)
plot(density(plot_d))
curve(dnorm(x, 0, 1), add = TRUE, col = "red")

# b: yes as the graphs showed that the difference between the 2 graphs were similar 
# which mean that our t test density is close to normal with only 6% being rejected
type1_error <- 0
for(i in 1:S){
  if(abs(plot_d[i]) > 1.96){
    type1_error <- type1_error + 1
  }
}
(type1_error/S) * 100

# c: because beta is moved by 0.1 we can see that our type 1 error become more frequent and we reject the null at a higher rate( 17.4%)
rm(list = ls())
set.seed(0)

S <- 1000 
n <- 100

beta <- 0.1
beta0 <- 0
alpha <- 0

betahat_ols <- numeric(S)
avar_betahat_ols <- numeric(S)

for (i in 1:S){
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  y <- alpha + beta * x + u
  
  ols_est <- lm(y~x)
  betahat_ols[i] <- coef(ols_est)["x"]
  alpha_est <- coef(ols_est)["(Intercept)"]
  uhat <- y - alpha_est - betahat_ols[i] * x
  varx <- mean((x - mean(x))^2)
  avar_betahat_ols[i] <- (1/n) * mean(uhat^2)/varx
}

ci_lower <- betahat_ols - 1.96 * sqrt(avar_betahat_ols)
ci_upper <- betahat_ols + 1.96 * sqrt(avar_betahat_ols)
reject <- as.logical((ci_lower>1) + (ci_upper<1)) 

1-mean(reject) 

plot_d <- (betahat_ols - beta0)/sqrt(avar_betahat_ols)
plot(density(plot_d))
curve(dnorm(x, 0, 1), add = TRUE, col = "red")

type1_error <- 0
for(i in 1:S){
  if(abs(plot_d[i]) > 1.96){
    type1_error <- type1_error + 1
  }
}
(type1_error/S) * 100

# d : yes as our beta becomes further away from zero we see that type 1 error increase much like we saw in the other parts
rm(list = ls())
set.seed(0)

S <- 1000 
n <- 100

beta0 <- 0
alpha <- 0

betahat_ols <- numeric(S)
avar_betahat_ols <- numeric(S)

beta_change <- function(beta){ 
  set.seed(0)
  for (i in 1:S){
    x <- rnorm(n, 0, 1)
    u <- rnorm(n, 0, 1)
    y <- alpha + beta * x + u
    
    ols_est <- lm(y~x)
    betahat_ols[i] <- coef(ols_est)["x"]
    alpha_est <- coef(ols_est)["(Intercept)"]
    uhat <- y - alpha_est - betahat_ols[i] * x
    varx <- mean((x - mean(x))^2)
    avar_betahat_ols[i] <- (1/n) * mean(uhat^2)/varx
  }
  
  plot_d <- (betahat_ols - beta0)/sqrt(avar_betahat_ols)
  plot(density(plot_d))
  curve(dnorm(x, 0, 1), add = TRUE, col = "red")
  
  type1_error <- 0
  for(i in 1:S){
    if(abs(plot_d[i]) > 1.96){
      type1_error <- type1_error + 1
    }
  }
  (type1_error/S) * 100
}

grid <- seq(from = -0.5, to = 0.5, by = 0.1) 
coverage <- sapply(X = grid,  FUN = beta_change)
plot(grid, coverage, main = "Difference in Beta and Type 1 Error %", xlab = "Beta",  ylab = " Type 1 Error %")
lines(grid, coverage, col =  "red")

