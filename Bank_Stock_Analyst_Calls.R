rm(list=ls())				# clearing

## Financial Data Analysis
library(quantmod)
## Get the daily traiding data for gs
getSymbols("GS",src="yahoo")
## What does the matrix gs include?
GS[1,]
candleChart(GS,multi.col=TRUE,theme="white")
## Log returns of the close price
gsrt = diff(log(Cl(GS)))
plot(gsrt,main="")

######## Apply GARCH Model #############################################
##divide into training and testing
## Predict July & August 
gsrt = gsrt[-1]
n=length(gsrt)
gsrt.train = gsrt[n-21:n]
gsrt.test =  gsrt[-c(n-21:n)]

## garchFit from the fGarch library
library(fGarch)
archFit.ts = garchFit(~ arma(5,6)+ garch(1,1), data=gsrt[-1], trace = FALSE)

library(rugarch)

### Goodness of Fit ####################################################
spec.1 = ugarchspec(variance.model=list(garchOrder=c(1,2)),
                    mean.model=list(armaOrder=c(5, 6), 
                                    include.mean=T), distribution.model="std")    
final.model.1 = ugarchfit(spec.1, gsrt, solver = 'hybrid')

fore = ugarchforecast(final.model.1, n.ahead=22)
fore.series.1 = NULL
fore.series.1 = c(fore.series.1, fore@forecast$seriesFor)



write.csv(fore.series.1, "C:/Users/anierudh/Desktop/imp_datintuit/Articles/Option Pricing/GS/output.csv")

## compare Information Criteria
infocriteria(final.model.1)


## Residual Analysis 
resids.final.model = residuals(final.model.1)
acf(resids.final.model,main="ACF of ARCH Residuals")
acf(resids.final.model^2,main="ACF of Squared ARCH Residuals")                    
Box.test(resids.final.model,lag=10,type='Ljung')
Box.test(resids.final.model^2,lag=10,type='Ljung')
qqnorm(resids.final.model)

### Prediction ##################################################

## 1. Prediction of the return time series
## 2. Prediction of the volatility 
nfore = length(gsrt.test)
fore.series.2 = NULL
fore.sigma.2 = NULL


for(f in 1: nfore){
  ## Fit models
  data = gsrt.train
  if(f>2)
    data = c(gsrt.train,gsrt.test[1:(f-1)])  
  final.model.2 = ugarchfit(spec.1, data, solver = 'hybrid')    
  ## Forecast
  fore = ugarchforecast(final.model.2, n.ahead=1)
  fore.series.2 = c(fore.series.2, fore@forecast$seriesFor)
  fore.sigma.2 = c(fore.sigma.2, fore@forecast$sigmaFor)
}


## Compute Accuracy Measures 

### Mean Squared Prediction Error (MSPE)
mean((fore.series.2 - gsrt.test)^2)

### Mean Absolute Prediction Error (MAE)
mean(abs(fore.series.2 - gsrt.test))

### Mean Absolute Percentage Error (MAPE)
mean(abs((fore.series.2 - gsrt.test)/gsrt.test))

### Precision Measure (PM)
sum((fore.series.2 - gsrt.test)^2)/sum((gsrt.test-mean(gsrt.test))^2)

ymin = min(c(as.vector(gsrt.test),fore.series.2))
ymax = max(c(as.vector(gsrt.test),fore.series.2))
data.plot = gsrt.test
names(data.plot)="Fore"
plot(gsrt[c(n-21):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Return Price")
data.plot$Fore=fore.series.2
lines(data.plot,lwd= 2, col="brown")

ymin = min(c(as.vector(gsrt.test^2),fore.sigma.2^2))
ymax = max(c(as.vector(gsrt.test^2),fore.sigma.2^2))

plot(gsrt[c(n-21):n]^2,type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Return Price")

data.plot$Fore=fore.sigma.2^2
lines(data.plot,lwd= 2, col="brown")

