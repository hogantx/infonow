setwd("C:/Users/bhogan/Documents/R/Data")
library(scales)
library(xts)
library(zoo)
library(tseries)
library(reshape)
library(forecast)
library(dplyr)
library(RODBC)
library(tidyr)

### load desktop data
infonow <- read.csv("infonow_desktop.csv")
k <- ncol(infonow)
device <- names(infonow)
infonow <- ts(infonow, freq=12, start=c(2013, 1))

### create empty data frame to paste forecast
test <- rep.int(0, 2*k)
fc <- matrix(test, nrow=2, ncol=k)
fc <- as.data.frame(fc)
names(fc) <- device

### file names for plot output
regions <- c("APAC", "Eastern Europe", "Greater China", "Latin America",
  "North America", "Western Europe")

par(mfrow=c(3,3))

### loop for generating a forecast
for(i in 1:k) {
	arima <- auto.arima(infonow[, i])
	forecast <- forecast(arima, h=2)
	fc[, i] <- forecast$mean
	plot(forecast, main=device[i])

	if( i%%9 == 0) {
		###create file name
		j <- i/9
		tmp <- paste0(regions[j], ".jpg")
		savePlot(tmp, "jpeg", device = dev.cur())


	}	
}

infonow_fc <- rbind(infonow, fc)
write.table(infonow_fc, "infonow_fc.csv")
