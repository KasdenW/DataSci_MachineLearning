######################################################################################
#question A pull in the data from production
#imported data set as textfile from copied word document/ calling the 
production
# seeing what the data looks like in the r environment 
head(production)
str(production)
hist(production$IP)
######################################################################################

# to get early understanding of the code im incorporating these functions to visual the data early on 
# normal statisitcal data anaylsis/ Qplot testting 
qqnorm(production$IP)
qqline(production$IP)

#checking to see if the H0 is normally distributed or not with the shapiro test
shapiro.test(production$IP)

#checking to the hypothesis test of the data
t.test(production$IP)
############################################


## Question B getting the sample period of the data within the range of 2010 and 2019

#converting the data to where i can access the data frame as a matrix
as.matrix.data.frame(production)
# restricting data down to 2010 to 2019 time frame
definedDataRange= production[1093:1203,]


######################################################################################
#### Question C 
# plot time series / ACF / PACF of IP
##library called for the function 
library(MASS)
library(ggplot2)
library(dplyr)
library(lmtest)
library(tseries)
library(egcn)
install.packages("orcutt")
library(orcutt)
install.packages("tseries")
library(tseries)
install.packages("aTSA")
library(aTSA)
####
#PlotTSS = ggplot(definedDataRange, aes(x=date, y=value)) + geom_line() + xlab("Date") + ylab("Industrial Production")
#plots the entire production for the total time frame
plot(production)
#only plots the portion from 2010 to 2019
ggplot(definedDataRange, aes(x= definedDataRange$date, y= definedDataRange$IP, group = 1)) + geom_line()
## plotting the ACF and PACF 
plotACF = acf(definedDataRange)
plotPACF = pacf(definedDataRange)

# looking for the stationary test at 5% stationary using ADF, PP, and KPSS tests for 110 
x <- arima.sim(list(definedDataRange = c(1,0,0),ar = 0.05),n = 110)
x
# Augmented Dickey-Fuller Test and comparison
stationary.test(x)
adf.test(x)
# Phillips–Perron test and comparison calc
stationary.test(x, method = "pp")
PP.test(x)

#Kwiatkowski-Phillips-Schmidt-Shin Test and comparison calc
stationary.test(x, method = "kpss")
kpss.test(x)

#############################################################################################
##### Question D
# generate all the things from but including a lag time

delta = diff(definedDataRange$IP, lag =1)
as.matrix.data.frame(delta)
summary(delta)
head(delta)
# graphing time series of the data defined range for the difference IP
plot(delta, type=  "l")
# time series lad plot of the ACF and PCF 
plotACF_DIFF = acf(delta)
plotPACF_DIFF = pacf(delta)

#setting up the Arima for the differenced delta of the data defined range / Changing the n count to 109 due to the lag change
Y <- arima.sim(list(delta = c(1,0,0),ar = 0.05),n = 109)
Y
# Augmented Dickey-Fuller Test and comparison
stationary.test(Y)
adf.test(Y)
# Phillips–Perron test and comparison calc
stationary.test(x, method = "pp")
PP.test(Y)

#Kwiatkowski-Phillips-Schmidt-Shin Test and comparison calc
stationary.test(x, method = "kpss")
kpss.test(Y)


##########################################################################################
#Question E
# Are there seasonal pattern and trend pattern in the series of IP?

# yes, i see a trend pattern associated with how the dow jones looks from the entire data spectrum, it shows a down turn 
# in certain years was due to economic downturn which was we saw in 2008, 2000 and many times throughout our history. 
# i find it interesting that the chart does closely follow the what the DJI and SPX looks like which makes sense. Overall
# production should match the economy. Like in a few years we will see what the COVID 19 black swan event has done to the economy. 
# I predict it will be much lower then what we saw in 2008 with the 10 treasury rates that have signalled that what we are currently in is far from the bottom.

#Regarding the defined region of 2010 to 2019, we can see the recovery of the markets under obamas term from 2010 and into 2012 where 
# we then see a regime change to Premier Trump. With the forecast I created in the last question, it cant fully comprehend the crisis that we are now,
# the data cant expand and predict a black swan event and relies on the date be efficient for the market model. 

# ooverall the trend is bullish and  looks to continue to do so 





##########################################################################################
g = rbind(delta,0)

#Question F 
#(f)	Use AIC or BIC to identify an optimal ARIMA model for the time series. 
#delta_definedDataRange= cbind(definedDataRange[1093:1203,],delta+1)
# one autoregressor 
TEST1 = arima(x=definedDataRange$IP, order=c(1,0,0), method = "ML")
summary(TEST2)
AIC(TEST1)
BIC(TEST1)


# one autoregressor and 1 differences value
#P = 1 , D = 1, 
TEST2 = arima(x=delta, order=c(1,1,0), method = "ML")
summary(TEST2)
AIC(TEST2)
BIC(TEST2)
###

##########################################################################################
#question G
# BOX TEST
# showing the serial correlation of the residuals for both 
Box.test(definedDataRange$IP- delta,lag = 1, type = "Ljung-Box")
# box test of each type with qq plot summary stats
Box.test(definedDataRange$IP,type = "Ljung-Box" )
Box.test(delta,type = "Ljung-Box" )
summary(delta)
qqnorm(delta)
qqline(delta)
checkresiduals(delta)
##########################################################################################

#Question H

library(PerformanceAnalytics)
install.packages("fpp2")
library(fpp2)

A = forecast(definedDataRange$IP, h= 12)
plot(A)
summary(A)
checkresiduals(A)





