# Get the working directory
getwd()
# Change the working directory
setwd("/Users/Tonkos/OneDrive/Documents/ECON114/Final Project")
install.packages("dplyr")
install.packages("lubridate")
install.packages("cobalt")
install.packages("jtools")
install.packages("stargazer")

library("ggplot2")
library("dplyr")
library("lubridate")
library("cobalt")
library("jtools")
library("stargazer")


# dataframe covid
# link to api: https://dev.socrata.com/foundry/data.cdc.gov/9mfq-cb36
###################################################################################

covid <- read.csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
covid_AZ <- covid[covid$state == "AZ",]
covid_NV <- covid[covid$state == "NV",]

#as.numeric(as.character(covid_AZ$pop))
covid_AZ$tot_cases_pop <- round(as.numeric(as.character(covid_AZ$tot_cases))/7276316, digits = 4)*100
covid_AZ$tot_death_pop <- round(as.numeric(as.character(covid_AZ$tot_death))/7276316, digits = 4)*100

covid_NV$tot_cases_pop <- round(as.numeric(as.character(covid_NV$tot_cases))/3143991, digits = 4)*100
covid_NV$tot_death_pop <- round(as.numeric(as.character(covid_NV$tot_death))/3143991, digits = 4)*100

covid_AZ_NV <- union(covid_AZ, covid_NV)

# setting up for time series for AZ and NV June 26, 2020 mask mandate 
covid_AZ_NV$submission_date <- lubridate::mdy(covid_AZ_NV$submission_date)
covid_AZ_NV <- dplyr::arrange(covid_AZ_NV, submission_date)

# making treatment
covid_AZ_NV$after <- ifelse(covid_AZ_NV$submission_date >= "2020-06-26", 1, 0)
covid_AZ_NV$treat <- ifelse(covid_AZ_NV$state == "NV", 1, 0)

# diff in diff
AZ_NV_didreg_tot_death_pop <- glm(tot_death_pop ~ treat + after + after*treat, data = covid_AZ_NV)
summ(AZ_NV_didreg_tot_death_pop, robust = TRUE)


AZ_NV_didreg_tot_cases_pop <- glm(tot_cases_pop ~ treat + after + after*treat, data = covid_AZ_NV)
summary(AZ_NV_didreg_tot_cases_pop, robust = TRUE)
summ(AZ_NV_didreg_tot_cases_pop, robust = TRUE)

stargazer(AZ_NV_didreg_tot_death_pop, AZ_NV_didreg_tot_cases_pop, type="html", out = "didreg.html")

# plot
xseq<-seq.Date(min(covid_AZ_NV$submission_date),max(covid_AZ_NV$submission_date),by='1 months') 
yseq<-round(seq(0,max(covid_AZ_NV$tot_cases_pop),by=1),0)

covid_AZ_NV$submission_date <- as.Date(covid_AZ_NV$submission_date)
plot(covid_AZ_NV$tot_cases_pop[covid_AZ_NV$treat == 1]~covid_AZ_NV$submission_date[covid_AZ_NV$treat == 1],
     type='o', ylim=c(0,max(covid_AZ_NV$tot_cases_pop)),
     xlim=c(min(covid_AZ_NV$submission_date), max(covid_AZ_NV$submission_date)), axes=F, col = "red", xlab = "Dates", 
     main = "Graph 1: COVID-19 Total Case divide by Population", ylab = "COVID-19 Total Case");box() 
axis.Date(side=1,at=xseq, format='%y-%m', labels=T,las=3)
axis(side=2,at=yseq,las=2)
points(covid_AZ_NV$tot_cases_pop[covid_AZ_NV$treat == 0]~covid_AZ_NV$submission_date[covid_AZ_NV$treat == 0], type='o',col = "blue")
abline(v = as.Date("2020-06-26"), lwd=2, lty=2)

# plot
xseq<-seq.Date(min(covid_AZ_NV$submission_date),max(covid_AZ_NV$submission_date),by='1 months') 
yseq<-seq(min(covid_AZ_NV$tot_death_pop),max(covid_AZ_NV$tot_death_pop),by=0.1)

covid_AZ_NV$submission_date <- as.Date(covid_AZ_NV$submission_date)
plot(covid_AZ_NV$tot_death_pop[covid_AZ_NV$treat == 1]~covid_AZ_NV$submission_date[covid_AZ_NV$treat == 1],
     type='o', ylim=c(0,max(covid_AZ_NV$tot_death_pop)),
     xlim=c(min(covid_AZ_NV$submission_date), max(covid_AZ_NV$submission_date)), axes=F, col = "red", xlab = "Dates", 
     main = "Graph 1: COVID-19 Total Death divide by Population", ylab = "COVID-19 Total Case");box() 
axis.Date(side=1,at=xseq, format='%y-%m', labels=T,las=3)
axis(side=2,at=yseq,las=2)
points(covid_AZ_NV$tot_death_pop[covid_AZ_NV$treat == 0]~covid_AZ_NV$submission_date[covid_AZ_NV$treat == 0], type='o',col = "blue")
abline(v = as.Date("2020-06-26"), lwd=2, lty=2)

###################################################################################

# Economic data AZ and NV unemployment
# Links: https://fred.stlouisfed.org/series/AZUR
# Links: https://fred.stlouisfed.org/series/NVUR
AZ_UP <- read.csv("AZUR.csv")
AZ_UP$UR <- AZ_UP$AZUR
AZ_UP$state <- "AZ"
AZ_UP <- subset(AZ_UP, select = -AZUR)

NV_UP <- read.csv("NVUR.csv")
NV_UP$UR <- NV_UP$NVUR
NV_UP$state <- "NV"
NV_UP <- subset(NV_UP, select = -NVUR)

AZ_NV_UP <- union(AZ_UP, NV_UP)

# setting up for time series for AZ and NV June 26, 2020 mask mandate 
AZ_NV_UP$DATE <- lubridate::ymd(AZ_NV_UP$DATE)
AZ_NV_UP <- dplyr::arrange(AZ_NV_UP, DATE)

# making treatment
AZ_NV_UP$after <- ifelse(AZ_NV_UP$DATE >= "2020-07-01", 1, 0)
AZ_NV_UP$treat <- ifelse(AZ_NV_UP$state == "NV", 1, 0)

# diff in diff
AZ_NV_UP_didreg <- glm(UR ~ treat + after + after*treat, data = AZ_NV_UP)
summary(AZ_NV_UP_didreg)

stargazer(AZ_NV_UP_didreg, type="html", out = "updidreg.html")

# Plot
xseq<-seq.Date(min(AZ_NV_UP$DATE),max(AZ_NV_UP$DATE),by='1 months') 
yseq<-round(seq(0,max(AZ_NV_UP$UR),by=1),0)

plot(AZ_NV_UP$UR[AZ_NV_UP$treat==1]~AZ_NV_UP$DATE[AZ_NV_UP$treat==1], 
     type='b', ylim=c(0,max(AZ_NV_UP$UR)),
     xlim=c(min(AZ_NV_UP$DATE), max(AZ_NV_UP$DATE)), axes=F,
     col = "red", xlab = "Date", main = "Graph3: Unemployment rate", ylab = "Unemployment rate");box() 
axis.Date(side=1,at=xseq, format='%y-%m', labels=T,las=3)
axis(side=2,at=yseq,las=2)
points( AZ_NV_UP$UR[AZ_NV_UP$treat==0]~AZ_NV_UP$DATE[AZ_NV_UP$treat==0], type='b', col = "blue")
abline(v = as.Date("2020-07-01"), lwd=2, lty=2)

#######################################################################################################################
# Economic data AZ and NV Labor Force Participation Rate 
# Links: https://fred.stlouisfed.org/series/LBSNSA32
# Links: https://fred.stlouisfed.org/series/LBSSA04
AZ_LFPR <- read.csv("LBSSA04.csv")
AZ_LFPR$LFPR <- AZ_LFPR$LBSSA04
AZ_LFPR$state <- "AZ"
AZ_LFPR <- subset(AZ_LFPR, select = -LBSSA04)

NV_LFPR <- read.csv("LBSNSA32.csv")
NV_LFPR$LFPR <- NV_LFPR$LBSNSA32
NV_LFPR$state <- "NV"
NV_LFPR <- subset(NV_LFPR, select = -LBSNSA32)

AZ_NV_LFPR <- union(AZ_LFPR, NV_LFPR)

# setting up for time series for AZ and NV June 26, 2020 mask mandate 
AZ_NV_LFPR$DATE <- lubridate::ymd(AZ_NV_LFPR$DATE)
AZ_NV_LFPR <- dplyr::arrange(AZ_NV_LFPR, DATE)

# making treatment
AZ_NV_LFPR$after <- ifelse(AZ_NV_LFPR$DATE >= "2020-07-01", 1, 0)
AZ_NV_LFPR$treat <- ifelse(AZ_NV_LFPR$state == "NV", 1, 0)


# diff in diff
AZ_NV_LFPR_didreg <- glm(LFPR ~ treat + after + after*treat, data = AZ_NV_LFPR)
summary(AZ_NV_LFPR_didreg)

stargazer(AZ_NV_LFPR_didreg, type="html", out = "LFPRdidreg.html")

# Plot
xseq<-seq.Date(min(AZ_NV_LFPR$DATE),max(AZ_NV_LFPR$DATE),by='1 months') 
yseq<-round(seq(min(AZ_NV_LFPR$LFPR),max(AZ_NV_LFPR$LFPR),by=1),0)

plot(AZ_NV_LFPR$LFPR[AZ_NV_LFPR$treat==1]~AZ_NV_LFPR$DATE[AZ_NV_LFPR$treat==1],
     type='b', ylim=c(min(AZ_NV_LFPR$LFPR), max(AZ_NV_LFPR$LFPR)),
     xlim=c(min(AZ_NV_LFPR$DATE), max(AZ_NV_LFPR$DATE)), axes=F,
     col = "red", xlab = "Date", main = "Graph 4: Labor Force Participation Rate", ylab = "Labor Force Participation Rate");box() 
axis.Date(side=1,at=xseq, format='%y-%m', labels=T,las=3)
axis(side=2,at=yseq,las=2)
points(AZ_NV_LFPR$LFPR[AZ_NV_LFPR$treat==0]~AZ_NV_LFPR$DATE[AZ_NV_LFPR$treat==0],type='b', col = "blue")
abline(v = as.Date("2020-07-01"), lwd=2, lty=2)

#####################################################################################################################  
# Balance Table for COVID-19 case
covid_AZ_NV_before <- data.frame(covid_AZ_NV$tot_cases_pop, covid_AZ_NV$tot_death_pop, covid_AZ_NV$state)
covid_AZ_NV_before <- covid_AZ_NV_before[1:314,]

bal.tab(covid_AZ_NV_before, treat = covid_AZ_NV_before$state, disp = c("diff", "means", "sds"))

# Balance Table for Unemployment 
AZ_NV_UP <- AZ_NV_UP[1:60,]
bal.tab(AZ_NV_UP, treat = AZ_NV_UP$state, disp = c("diff", "means", "sds"))

# Balance Table for Labor Force Participation Rate
AZ_NV_LFPR <- AZ_NV_LFPR[1:60,]
bal.tab(AZ_NV_LFPR, treat = AZ_NV_LFPR$state, disp = c("diff", "means", "sds"))
stargazer(table, type="html", out = "tab.html")

# test divide all number by population
AZ <- read.csv("AZ.csv")
NV <- read.csv("NV.csv")

AZ <- AZ[1:47,]
NV <- NV[1:47,]

AZ <- subset(AZ, select = -c(Value.Note.for.Arizona))
NV <- subset(NV, select = -c(Value.Note.for.Nevada))

AZ_NV <- data.frame(pop = c(AZ$Arizona[1], NV$Nevada[1]), pop_percent_change = c(AZ$Arizona[3], NV$Nevada[3]),
                    Persons_under_5_year = c(AZ$Arizona[6], NV$Nevada[6]), Persons_under_18_years = c(AZ$Arizona[7], NV$Nevada[7]),
                    Persons_under_65_years_over = c(AZ$Arizona[8], NV$Nevada[8]), Female = c(AZ$Arizona[9], NV$Nevada[9]), treat = c(0,1))

AZ_NV_bal_tab <- data.frame()

table <- bal.tab(AZ_NV, treat = AZ_NV$treat)
stargazer(table, type="html", out = "az_nv.html")

