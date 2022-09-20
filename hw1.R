# Quuestion 1

# A: we get closer to the µ as the sample size gets bigger
n1 <- 10
n2 <- 100
n3 <- 1000
n4 <- 10000

a <- 2
b <- 4

set.seed(0)

mean(rnorm(n1, a, b))
mean(rnorm(n2, a, b))
mean(rnorm(n3, a, b))
mean(rnorm(n4, a, b))

# B: The sd get closer to 0 as the sample size increase because sd equation is divide by the n population thus the larger the n is the lower the sd becomes
S <- 10000

mean1 <- numeric(S)
mean2 <- numeric(S)
mean3 <- numeric(S)
mean4 <- numeric(S)

for (i in 1:S){
  sample <- runif(n1, a, b)
  mean1[i] <- mean(sample) 
}

for (i in 1:S){
  sample <- runif(n2, a, b)
  mean2[i] <- mean(sample) 
}

for (i in 1:S){
  sample <- runif(n3, a, b)
  mean3[i] <- mean(sample) 
}

for (i in 1:S){
  sample <- runif(n4, a, b)
  mean4[i] <- mean(sample) 
}

sd(mean1)
sd(mean2)
sd(mean3)
sd(mean4)

# C: we observe that all the graph are normally distributed at mean = 3.0 but varies in how stretch is the tail is from the center 
par(mfrow=c(2,2)) 
plot(density(mean1))
plot(density(mean2))
plot(density(mean3))
plot(density(mean4)) 

# D: graphs don't look good
mean1_t <- numeric(S)
mean2_t <- numeric(S)
mean3_t <- numeric(S)
mean4_t <- numeric(S)

for (i in 1:S){
  sample <- rt(n1, 1, 0)
  mean1_t[i] <- mean(sample) 
}

for (i in 1:S){
  sample <- rt(n2, 1, 0)
  mean2_t[i] <- mean(sample) 
}

for (i in 1:S){
  sample <- rt(n3, 1, 0)
  mean3_t[i] <- mean(sample) 
}

for (i in 1:S){
  sample <- rt(n4, 1, 0)
  mean4_t[i] <- mean(sample) 
}

sd(mean1_t)
sd(mean2_t)
sd(mean3_t)
sd(mean4_t)  

par(mfrow=c(2,2))
plot(density(mean1_t))
plot(density(mean2_t))
plot(density(mean3_t))
plot(density(mean4_t)) 

# Question 2

# A:
rm(list = ls())
set.seed(0)

S <- 1000
n <- 100
k <- 4
clt <- numeric(S)

for (i in 1:S){
  clt[i] <- sqrt(n)*(mean(rchisq(n, k))-k)/sqrt(8)
}


# B:
set.seed(0)

clt2 <- numeric(S)

for (i in 1:S){
  clt2[i] <- n^(1/4)*(mean(rchisq(n, k))-k)/sqrt(8)
}

# C:
set.seed(0)

clt3 <- numeric(S)

for (i in 1:S){
  clt2[i] <- sqrt(n)*(mean(rchisq(n, k))-3)/sqrt(8)
}

par(mfrow=c(2,2))
plot(density(clt))
plot(density(clt2))
plot(density(clt3))
# first one which is the correct one and the incorrect rate are similar but the centering is off while the incorrect centering seem to look like a normal distribution 


# Question 3
# A: credit to https://www.youtube.com/watch?v=4DxgO7c3Zgw
set.seed(0)
S <- 100000000
x <- runif(S)
y <- runif(S)
rm <- (sqrt((x)^2 +(y)^2))
n1 <- rm[rm < 1]
p <- 4 * (length(n1)/S)
p

# Question 4
# A: the value of ??1 is -??0 - Vi - (Ui/2)
# B & C ??1 seems to be pretty consistent 
#> mean(gamma1_hat100)
#[1] 0.4020841
#> mean(gamma1_hat500)
#[1] 0.399333
#> mean(gamma1_hat1000)
#[1] 0.4012135

n <- 500
n1 <- 100
n2 <- 1000
S <- 100
b0 <- 1
b1 <- 2

gamma1_hat100 <- numeric(S)
gamma1_hat500 <- numeric(S)
gamma1_hat1000 <- numeric(S)

for (i in 1:S) {
  U <- rnorm(n1, 0, 1)
  X <- rnorm(n1, 0, 1)
  Y <- b0 + b1 * X + U
  gamma_hats <- coefficients(lm(X ~ Y))
  gamma1_hat100[i] <- gamma_hats["Y"]
}

for (i in 1:S) {
  U <- rnorm(n, 0, 1)
  X <- rnorm(n, 0, 1)
  Y <- b0 + b1 * X + U
  gamma_hats <- coefficients(lm(X ~ Y))
  gamma1_hat500[i] <- gamma_hats["Y"]
}

for (i in 1:S) {
  U <- rnorm(n2, 0, 1)
  X <- rnorm(n2, 0, 1)
  Y <- b0 + b1 * X + U
  gamma_hats <- coefficients(lm(X ~ Y))
  gamma1_hat1000[i] <- gamma_hats["Y"]
}

par(mfrow=c(2,2))
plot(density(gamma1_hat100))
plot(density(gamma1_hat500))
plot(density(gamma1_hat1000))

mean(gamma1_hat100)
mean(gamma1_hat500)
mean(gamma1_hat1000)

# Question 5
getwd()
# Change the working directory
setwd("/Users/Tonkos/OneDrive/Documents/ECON114")
install.packages("wooldridge")
library(wooldridge)

reg <- lm(rental$lrent[rental$year == 90] ~ rental$lpop[rental$year == 90] + rental$lavginc[rental$year == 90] + rental$pctstu[rental$year == 90], data = rental)  
summary(reg)
# A: H0: ß3 = 0, Ha: ß3 != 0 

# B: I expect both to be positive

# C: the statement is wrong because we using log rent therefore 10% doesn't increase rent by 6.6%
#
#lm(formula = rental$lrent[rental$year == 90] ~ rental$lpop[rental$year == 
#                                                             90] + rental$lavginc[rental$year == 90] + rental$pctstu[rental$year == 
#                                                                                                                       90], data = rental)
#
#Coefficients: The statement using model doesnt not give the same answer as it does not account for population to be a log
#  (Intercept)     rental$lpop[rental$year == 90]  rental$lavginc[rental$year == 90]   rental$pctstu[rental$year == 90]  
#0.04278                            0.06587                            0.50702                            0.00563  

# D: 
#One Sample t-test
#
#data:  rental$pctstu[rental$year == 90]
#t = 16.358, df = 63, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#99 percent confidence interval:
# 23.32613 32.36959
#sample estimates:
#  mean of x 
#27.84786
t.test(rental$pctstu[rental$year == 90], mu = 0, conf.level = 0.99)
