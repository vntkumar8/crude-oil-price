# Loading
library("readxl")
library("ggplot2")
library("xts")
library("fpp2")
library("dplyr")
library("rugarch")
library("fGarch")
library(zoo)
# xls files
my_data <- read_excel("Data/Crude Oil Prices Daily.xlsx")
head(my_data)
dim(my_data)
## considering data from year 2000

data=my_data[3551:8223,]

price = data.frame(data[,2])

length(price[is.na(price)])  ## no. of missing values

## imputing missing values using last observation ####
dat=na.locf(price, option = "nocb")

length(dat[is.na(dat)]) # no missing value

ts_data=ts(dat,start=2000, frequency = 365.25)



autoplot(ts_data) + ggtitle("Crude oil price fluctuation") + xlab("Year") + 
  ylab("Price")


ggseasonplot(ts_data, year.labels=TRUE, year.labels.left=TRUE) +
  ggtitle("Price fluctuation yearwise")+ xlab("Days") + ylab("Price")

gglagplot(ts_data)

ggAcf(ts_data)

## Decomposition

ts_data %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
          of electrical equipment index")

### 5-MA series
autoplot(ts_data,series = "Data") + autolayer(ma(ts_data,5), series = "5-MA")+
  xlab("Year") + ylab("Crude Oil price")+ ggtitle("MA-5 series")



train = ts_data[1:4537,]
test=ts_data[4538:4673,]
length(test)

ex=ses(train,h=136,alpha=0.1, initial = "simple")
ex

#round(accuracy(test,e),2)

autoplot(ex) + autolayer(fitted(ex), series="Fitted")

## Applying Arima
fit<-auto.arima(train, stepwise = FALSE)
fc=forecast(fit,h=136)
## summary of the model ####

summary(fit)

autoplot(fc)
### checking the residual ####
### From the ACF plot, we can say there is significant correlation in residuals ###

### So, residuals are not white noise
checkresiduals(fit)

### Applying SARIMA ###
fit_s<-auto.arima(train,  seasonal = T,stepwise = FALSE)
fc_s=forecast(fit_s,h=136)
## summary of the model ####
## No seasonality found
summary(fit_s)

## plot ##

autoplot(fc_s)


## Check residuals ###

checkresiduals(fit_s)

## metric ##


### TBATS for multiple seasonality #####

fit_t= tbats(train)

fc_t=forecast(fit_t,h=136)

### summary ###

summary(fit_t)

### Metric ####


###################### garch ####################



oilfit= ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")


garch11.fit=ugarchfit(spec=oilfit, data=train)
garch11.fit   ## AIC And BIC much lesser than Arima's AIC and BIC

garchforecast1 <- ugarchforecast(garch11.fit , n.ahead = 136, data = test)

pred=fitted(garchforecast1)
accuracy(test,pred)

plot(garch11.fit, which=9)  ## Q-Q plot  standardised residual is not normal


oilfit1= ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
garch11.fit1=ugarchfit(spec=oilfit1, data=train)
garch11.fit1  ## AIC and BIC is better than the previous garch model

garchforecast2 <- ugarchforecast(garch11.fit1 , n.ahead = 136, data = test)

pred=fitted(garchforecast2)
accuracy(test,pred) #Both MAPE and RMSE is lesser for this one than the earlier one.


plot(garch11.fit1, which=9)  ## Q-Q plot  standardised residual is not normal

###################### don't add in the report ####################
xx=(diff(ts_data)/ts_data)*100  ## modeling percentage change in oil price
xx1=na.omit(xx)

oilfit= ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")

garch11.fit=ugarchfit(spec=oilfit, data=xx1)
garch11.fit   ## AIC And BIC much lesser than Arima's AIC and BI

plot(garch11.fit, which=9)  ## Q-Q plot  standardised residual is almost normal

################################################################3

#===Arima Regression=============================================#

install.packages("tsDyn")
library('tsDyn')

mod<-(aar(train, m=3))
plot(mod)
res.mod<-predict_rolling(mod, newdata=test)
accuracy_stat(res.mod)
#========Arima Regression

yen<-read.csv('/Data/dollar-yen-exchange-rate-historical-chart_2000-2018.csv')
yen_ts<-(ts(yen$value,start=2000,frequency=365.25))
train_yen=yen_ts[1:4537]
test_yen=yen_ts[4538:4673]

fit <- auto.arima(train, xreg=train_yen)
summary(fit)

#===

yen<-read.csv('/Data/pound-dollar-exchange-rate-historical-chart_2000-2018.csv')
yen_ts<-(ts(yen$value,start=2000,frequency=365.25))
train_yen=yen_ts[1:4537]
test_yen=yen_ts[4538:4673]

fit <- auto.arima(train, xreg=train_yen)
summary(fit)


#=====

