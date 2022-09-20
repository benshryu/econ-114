rm(list = ls()) 
install.packages("stargazer")
library(wooldridge)
library(stargazer)

# Question 1
# a
data("cps78_85", package = "wooldridge")
a <- glm(lwage ~ y85 * (educ + female) + exper + I(exper^2) + union, data = cps78_85) 
summary(a)
stargazer(a, type="text")

# b: y85 is binary on whether or not it was the year 1985, 
# and to interpret y85 there is a  0.075 percentage change in wage when y85 is true
# difference between wether or not the person y85 is true,
# y85 * educ and y85 * female interaction term is only applied both statment are true or 1
# when a person is y85(year 1985) * educ(whether are not education is true) there is an 0.018 percentage incease in wage
# when a person is y85 * female(whether are not they are a female) there is 0.085 percentage increase in wage

# c; considering that a person is a male with twelve years of education we can plug this into our regression
cps78_85$educ_12 <- cps78_85$educ - 12
c <- glm(lwage ~ y85 + educ + y85 * (educ_12 + female) + exper + I(exper^2)  + union, data = cps78_85) 
stargazer(c, type="text")
# we get that the wage growth is about 33.9 percentage change for a male with twelve years of education

# d
cps78_85$vec1 <- rep(0, 1084)
cps78_85$vec1[cps78_85$year == 78] <- 0 
cps78_85$vec1[cps78_85$year == 85] <- log(1.65)
cps78_85$lrwage <- cps78_85$lwage - cps78_85$vec1

d <- glm(lrwage ~ y85*(educ+female) + exper + I(exper^2) + union, data = cps78_85)
stargazer(a, d, type="text")

# e: dropped from 30.55% to 17.98%
summary(cps78_85$union[cps78_85$year == 78])
summary(cps78_85$union[cps78_85$year == 85])

# f: union did not change from a to f remaining at 0.202
f <- glm(lwage ~ y85*(educ + female + union) + exper + I(exper^2), data = cps78_85) 

stargazer(a, f, type="text")

# g: no this just means that union particiaption decrease but the effect of union on wage remained samed

# Question 2 
rm(list = ls())
install.packages("stargazer")
# a: y81*log(dist) mean that a 1 percentage increace in distance and 
# if y81 is true we are going see the effect of price of a house by delta1 percentage change
# delta1 is going to be a postive sign as the distance increases from the incinerator the more expansive the price of housing is
# and if beta1 is greater 0 it means that the distance is greater then 0 percentage increase

# b: when y81 is true and distance is increase by 1 percentage we are to see a 0.048 percentage increase in price
# and is not statistically significant 
data("kielmc", package = "wooldridge")
b <- glm(lprice ~ ldist*y81, data=kielmc)
library(stargazer)
stargazer(b, type="text")

# c: we can see that the distance from the incenerator does not prove to statistically significant on the house value
# other variable has more of an impact when we control for them
c <- glm(lprice ~ ldist*y81 + age + I(age^2) + log(intst) + log(land) + log(area) + rooms + baths, data = kielmc)
stargazer(b, c, type="text")

# Question 3
rm(list = ls())
install.packages("plm")
library(plm);library(lmtest)
data("jtrain", package = "wooldridge")

# a: 157 firms and 471 total observations
dfp <- pdata.frame(jtrain, index=c("fcode", "year"))
pdim(dfp)
table(jtrain$year)

# b: grant beta1 is 32.626 at a p<0.01(statistically significant at the 1% level). which means when a firm has a grants
# they increase hours of job training by 32.626 hours on average
b <- plm(hrsemp ~ factor(year) + grant + grant_1 + lemploy, data = dfp, model="fd")
stargazer(b, type = "text")

# c: No, because it likely that a company will not shorten the training hours based on the grants of the last year
# reasonable due impact grants in the concurrent year has over grants that no longer exist

# d: the bigger firms tend to train more hours but by 0.166 per percentage increase but this is not statistically significant enough
# there for the size of the firm does not have an impact on the training hours

# Question 4
rm(list = ls())
install.packages("stargazer")
install.packages("plm")

# a: if excution has a detterent effect the sign of excec should be negative as it would decrease the the murder rate
# while unemployment should increase crime in general which include murder rates, thus the sign for unem is postive

# b: the data suggest that excution has a postive increase in murder rate is not statistically significant enough
# we do not find that excution has a detterent effect
library(plm);library(lmtest); library(stargazer)
data("murder", package = "wooldridge")

murder_subset_9093 <- subset(murder, year!=87)
b <- glm(mrdrte ~ exec + unem + d93, data = murder_subset_9093) 
stargazer(b, type="text")

# c
coeftest(b, vcovHC)

# d: Texas has the largest number for the execution variable in 1993 and the next highest being 11
murder$state[murder$exec == max(murder$exec[murder$year == 93])]
max(murder$exec[murder$year==93]) 
table(murder$exec[murder$year==93])

# e: se for robust is smaller, the varriance of the error is diffrent as across the x-axis/states which is why we use robust se
library(plm);library(dplyr)
murder_subset_9093_tx <- murder_subset_9093[murder_subset_9093$state!="TX",]
murder_subset_9093_txp <- pdata.frame(murder_subset_9093_tx, index=c("state", "year"))
pdim(murder_subset_9093_txp)

e <- plm(mrdrte ~  exec + unem + d93, data = murder_subset_9093_txp, model = "fd")
e2 <- plm(mrdrte ~  exec + unem + d93 , data = murder_subset_9093_txp, model = "fd")

cse = function(x){
  i = sqrt(diag(vcovHC(x, type="HC0")))
  return(i)
}

stargazer(e, e2, type = "text", se = list(NULL, cse(e2)), column.labels=c("non-robust", "robust"))

# f: deterrent effect is bigger in magnitude and statistically significant at 10% level
murderp <-  pdata.frame(murder, index=c("state","year"))
pdim(murderp)

f <- plm(mrdrte ~ exec + unem + factor(year), data=murderp, model = "within")
f2 <- plm(mrdrte ~ exec + unem + factor(year), data=murderp, model = "within")

stargazer(f, f2, type = "text", se = list(NULL, cse(f2)), column.labels = c("non-robust", "robust"))

