setwd("C:\\Users\\vasil\\Dropbox\\Dropbox\\data")
require(xlsx)
require(reshape2) 
require(fpp)
require(forecast)
data=read.xlsx("PT_Tourism_TS.xlsx", sheetName="data")

# Converting special chars to UTF-8
data$location<-iconv(data$location, "UTF-8", "UTF-8")

# Dropping the unnecessary columns - date_orig
data <- data[, ! names(data) %in% c("date_orig","NA."), drop = F]
nights<- data[, ! names(data) %in% c("guests","incomes","NA.") , drop = F]

attach(data)
# Transposing
nights<-dcast(nights, date.vba. ~ location, value.var="nights", fill=0)

vars=names(nights)[2:length(names(nights))]
vars
detach(data)
#Moving Averages of nights
manights_Alc=ma(nights_Alc,order=12)
manights_Alg=ma(nights_Alg,order=12)
manights_AML=ma(nights_AML,order=12)
manights_Cen=ma(nights_Cen,order=12)
manights_Con=ma(nights_Con,order=12)
manights_Nor=ma(nights_Nor,order=12)
manights_RAM=ma(nights_RAM,order=12)
manights_RAA=ma(nights_RAA,order=12)

#########
#ARIMA models
#########

# Arima model for Alentejo
nights_Alc=ts(nights$Alentejo,start=c(2006,1),frequency=12)

plot(nights_Alc)
seasonplot(nights_Alc)
tsdisplay(nights_Alc)

#modifiations of Alc
plot(log(nights_Alc))
tsdisplay(diff(log(nights_Alc),12))
tsdisplay(diff(log(nights_Alc)))
tsdisplay(diff(diff(log(nights_Alc),12)))
tsdisplay(diff(diff(diff(log(nights_Alc),12),12)))
#it is stationary
adf.test(log(nights_Alc))

tsdisplay(log(nights_Alc))

fit_Alc3 = Arima(log(nights_Alc),order=c(1,0,0), seasonal=c(1,1,2))
fit_Alc4 <- Arima(log(nights_Alc), order=c(1,0,1), seasonal=c(0,1,1)) #auto.arima
fit_Alc2 <- Arima(log(nights_Alc), order=c(1,1,0), seasonal=c(1,2,2))
fit_Alc <- Arima(log(nights_Alc),order=c(3,1,0),seasonal=c(1,2,2))
fit_Alc4 #-208.63
fit_Alc3 #-172#Seems good
fit_Alc2 #-125
fit_Alc  #-138#Seems better

plot((log(nights_Alc)))
lines((log(nights_Alc))-fit_Alc$residuals, col="blue") 
lines((log(nights_Alc))-fit_Alc3$residuals, col="red")
lines((log(nights_Alc))-fit_Alc2$residuals, col="green")
lines((log(nights_Alc))-fit_Alc4$residuals, col="grey")

tsdiag(fit_Alc)
tsdisplay(fit_Alc$residuals)
adf.test(fit_Alc$residuals)
summary(fit_Alc)
cor(fitted(fit_Alc),log(nights_Alc) )
plot(forecast(fit_Alc, hr=24))
fore_Alc=forecast(fit_Alc)
residuals_Alc=fit_Alc$residuals
forecast_Alc = data.frame(matrix(unlist(fore_Alc[4]), nrow=24, byrow=T)) # make the df 
forecast_Alc=ts(exp(forecast_Alc),start=c(2016,2),frequency=12) # make the time series

y_Alc=c(as.numeric(nights_Alc),as.numeric(forecast_Alc))
final_Alc=ts(y_Alc,start=c(2006,1),frequency=12)
plot(final_Alc, col="blue")
lines(nights_Alc, col="black")

plot(decompose(final_Alc))



######### Arima model for Algarve 
nights_Alg=ts(nights$Algarve,start=c(2006,1),frequency=12)
tsdisplay(nights_Alg) # Additive seasonality , no trend
seasonplot(nights_Alg) # Seasonality [May,October]

#log transformation
plot(log(nights_Alg))
adf.test(log(nights_Alg), alternative=c("stationary"))
tsdisplay(log(nights_Alg))
tsdisplay(diff(log(nights_Alg),12))
tsdisplay(diff(log(nights_Alg)))
tsdisplay(diff(diff(log(nights_Alg),12)))

fit_Alg=Arima(log(nights_Alg), order=c(0,1,1), seasonal=c(1,1,1))
fit_Alg2=Arima(log(nights_Alg), order=c(2,0,0), seasonal=c(2,1,0))
fit_Alg #-243.56
fit_Alg2 #-243.56


plot(log(nights_Alg))
lines(log(nights_Alg)-fit_Alg$residuals, col="red")
lines(log(nights_Alg)-fit_Alg2$residuals, col="green")
adf.test(fit_Alg2$residuals, alternative=c("stationary"))

tsdiag(fit_Alg2)

tsdisplay(fit_Alg2$residuals)
residuals_Alg=fit_Alg2$residuals
summary(fit_Alg2)


plot(forecast(fit_Alg2, hr=24))
fore_Alg=forecast(fit_Alg2)


forecast_Alg = data.frame(matrix(unlist(fore_Alg[4]), nrow=24, byrow=T)) # make the df 
forecast_Alg=ts(exp(forecast_Alg),start=c(2016,2),frequency=12) # make the time series

y_Alg=c(as.numeric(nights_Alg),as.numeric(forecast_Alg))
final_Alg=ts(y_Alg,start=c(2006,1),frequency=12)
plot(final_Alg, col="blue")
lines(nights_Alg, col="black")

plot(decompose(final_Alg))


####### Arima model for Área Metropolitana de Lisboa

nights_AML=ts(nights$"Área Metropolitana de Lisboa",start=c(2006,1),frequency=12)
plot(nights_AML)
tsdisplay(nights_AML) # Take care of that 
seasonplot(nights_AML) #trend , seasonality [May] , [July,October]

#log transformation
plot(log(nights_AML))
adf.test(log(nights_AML))
tsdisplay(log(nights_AML))
tsdisplay(diff(log(nights_AML),12))
tsdisplay(diff(diff(log(nights_AML),12),12))



fit_AML=Arima(log(nights_AML),order=c(1,0,0),seasonal=c(1,1,0))
fit_AML2=Arima(log(nights_AML),order=c(3,0,0),seasonal=c(2,2,1))
fit_AML #299
fit_AML2 #-322.63


plot(log(nights_AML))
lines(log(nights_AML)-fit_AML$residuals, col="RED")
lines(log(nights_AML)-fit_AML2$residuals, col="blue")


adf.test(fit_AML2$residuals, alternative=c("stationary"))
summary(fit_AML2)
residuals_AML=fit_AML2$residuals
tsdisplay(fit_AML2$residuals)
tsdiag(fit_AML)
plot(forecast(fit_AML2, hr=24))

fore_AML=forecast(fit_AML2)
forecast_AML = data.frame(matrix(unlist(fore_AML[4]), nrow=24, byrow=T)) # make the df 
forecast_AML=ts(exp(forecast_AML),start=c(2016,2),frequency=12) # make the time series

y_AML=c(as.numeric(nights_AML),as.numeric(forecast_AML))
final_AML=ts(y_AML,start=c(2006,1),frequency=12)
plot(final_AML, col="blue")
lines(nights_AML, col="black")

plot(decompose(final_AML))

######## Arima model for Centro

nights_Cen=ts(nights$Centro,start=c(2006,1),frequency=12)
plot(nights_Cen)
tsdisplay(nights_Cen)
seasonplot(nights_Cen) # Seasonality [July, October]

#Log transformation
plot(log(nights_Cen))
adf.test(log(nights_Cen))

tsdisplay(log(nights_Cen))
tsdisplay(diff(diff(log(nights_Cen),12)))
tsdisplay(diff(log(nights_Cen),12))

auto.arima(log(nights_Cen))


fit_Cen2=Arima((log(nights_Cen)),order=c(1,0,1),seasonal=c(0,1,1)) #autoarima
fit_Cen4=Arima((log(nights_Cen)),order=c(1,0,0),seasonal=c(1,1,2))
fit_Cen5=Arima((log(nights_Cen)),order=c(0,1,1),seasonal=c(1,1,1))

fit_Cen2 #-255.59
fit_Cen4 #-236.85
fit_Cen5 #-251.79

plot(log(nights_Cen))
lines(log(nights_Cen)-fit_Cen5$residuals, col="red")
lines(log(nights_Cen)-fit_Cen4$residuals, col="blue")
adf.test(fit_Cen5$residuals)

summary(fit_Cen5)

tsdiag(fit_Cen5)

plot(forecast(fit_Cen5, hr=24))


residuals_Cen=fit_Cen5$residuals
fore_Cen=forecast(fit_Cen5)
forecast_Cen = data.frame(matrix(unlist(fore_Cen[4]), nrow=24, byrow=T)) # make the df 
forecast_Cen=ts(exp(forecast_Cen),start=c(2016,2),frequency=12) # make the time series

y_Cen=c(as.numeric(nights_Cen),as.numeric(forecast_Cen))
final_Cen=ts(y_Cen,start=c(2006,1),frequency=12)
plot(final_Cen, col="blue")
lines(nights_Cen, col="black")

plot(decompose(final_Cen))


############Arima for Norte
nights_Nor=ts(nights$Norte,start=c(2006,1),frequency=12)
seasonplot(nights_Nor) # trend(or cycle) , [June,Sep]
plot(nights_Nor)
tsdisplay(nights_Nor)

#log transformation
plot(log(nights_Nor))

adf.test(log(nights_Nor))

tsdisplay(log(nights_Nor))
tsdisplay(diff(log(nights_Nor),12))
tsdisplay(diff(diff(log(nights_Nor),12)))

fit_Nor1=Arima((log(nights_Nor)),order=c(2,0,0),seasonal=c(1,1,0)) 
fit_Nor2=Arima((log(nights_Nor)),order=c(0,1,1),seasonal=c(1,1,1))
  
fit_Nor4=Arima((log(nights_Nor)),order=c(2,0,1),seasonal=c(0,1,1))

fit_Nor5=Arima((log(nights_Nor)),order=c(0,1,1),seasonal=c(2,1,1)) 
fit_Nor6=Arima((log(nights_Nor)),order=c(0,1,1),seasonal=c(1,1,1))

fit_Nor1 #Very good estimate
fit_Nor2 #2nd best
fit_Nor4 #-302.49
fit_Nor5 #-300.64
fit_Nor6 # -302.63
plot(log(nights_Nor))
lines(log(nights_Nor)-fit_Nor2$residuals, col="red")
lines(log(nights_Nor)-fit_Nor3$residuals, col="green")
lines(log(nights_Nor)-fit_Nor6$residuals, col="purple")

tsdiag(fit_Nor1)
tsdisplay(fit_Nor1$residuals)
adf.test(fit_Nor1$residuals, alternative = "stationary")
residuals_Nor=fit_Nor1$residuals
summary(fit_Nor1)

plot(forecast(fit_Nor1, hr=24))


fore_Nor=forecast(fit_Nor1)
forecast_Nor = data.frame(matrix(unlist(fore_Nor[4]), nrow=24, byrow=T)) # make the df 
forecast_Nor=ts(exp(forecast_Nor),start=c(2016,2),frequency=12) # make the time series

y_Nor=c(as.numeric(nights_Nor),as.numeric(forecast_Nor))
final_Nor=ts(y_Nor,start=c(2006,1),frequency=12)
plot(final_Nor, col="blue")
lines(nights_Nor, col="black")

plot(decompose(final_Nor))

  
  ##########Arima RAA . Açores
  

  nights_RAA=ts(nights$"Região Autónoma dos Açores" ,start=c(2006,1),frequency=12)
  plot(nights_RAA)
  
  seasonplot(nights_RAA)
  tsdisplay(nights_RAA)

  
tsdisplay(log(nights_RAA))  

  tsdisplay(diff(log(nights_RAA),12))

  adf.test(log(nights_RAA))

  
fit_RAA2=Arima((log(nights_RAA)),order=c(1,0,0),seasonal=c(1,1,0))
fit_RAA3=Arima((log(nights_RAA)),order=c(1,0,0),seasonal=c(1,1,0))
fit_RAA6=Arima((log(nights_RAA)),order=c(1,0,0),seasonal=c(1,2,0))
fit_RAA4=Arima((log(nights_RAA)),order=c(2,0,0),seasonal=c(2,1,1))
fit_RAA5=Arima((log(nights_RAA)),order=c(1,2,1),seasonal=c(1,1,0))
fit_RAA7=Arima((log(nights_RAA)),order=c(1,0,0),seasonal=c(0,1,1))

fit_RAA2#-221.5
fit_RAA6#-135.9
fit_RAA2#-220.7
fit_RAA3#-220.44
fit_RAA4#-220.85
fit_RAA5#-203.19


plot((log(nights_RAA)))
lines(log(nights_RAA)-fit_RAA2$residuals,col="blue")    
plot(forecast(fit_RAA2, hr=24))

tsdiag(fit_RAA2)
  

tsdisplay(fit_RAA2$residuals)
adf.test(fit_RAA2$residuals)
residuals_RAA=fit_RAA2$residuals
   summary(fit_RAA2)
  fore_RAA=forecast(fit_RAA2)
  fore_RAA
  forecast_RAA = data.frame(matrix(unlist(fore_RAA[4]), nrow=24, byrow=T)) # make the df 
  forecast_RAA=ts(exp(forecast_RAA),start=c(2016,2),frequency=12) # make the time series
  
  y_RAA=c(as.numeric(nights_RAA),as.numeric(forecast_RAA))
  final_RAA=ts(y_RAA,start=c(2006,1),frequency=12)
  plot(final_RAA, col="blue")
  lines(nights_RAA, col="black")
  
  plot(decompose(final_RAA))
  
  
  ##########Arima RAM . Madeira
  
  nights_RAM=ts(nights$"Região Autónoma da Madeira" ,start=c(2006,1),frequency=12)
  plot(nights_RAM)
  seasonplot(nights_RAM)
  
  tsdisplay(nights_RAM)
  
  plot(log(nights_RAM))
  adf.test(log(nights_RAM))
  
  tsdisplay(log(nights_RAM))
  
  tsdisplay(diff(log(nights_RAM),12))
  
  auto.arima(log(nights_RAM))
 
  fit_RAM=Arima((log(nights_RAM)),order=c(1,0,0),seasonal=c(1,1,0))
  
  fit_RAM #-262.53
  fit_RAM2 #-333.03
  fit_RAM3 #-331.04
  fit_RAM4 #-340.23 1,0,0)(1,1,0
  
  plot(log(nights_RAM))
  lines(log(nights_RAM)-fit_RAM$residuals, col="red")
 
  
  tsdiag(fit_RAM)
  tsdisplay(fit_RAM$residuals)
  adf.test(fit_RAM$residuals, alternative = "stationary")
  
  summary(fit_RAM)
  residuals_RAM=fit_RAM$residuals
  

  plot(forecast(fit_RAM, hr=24))
  
  fore_RAM=forecast(fit_RAM)
  forecast_RAM = data.frame(matrix(unlist(fore_RAM[4]), nrow=24, byrow=T)) # make the df 
  forecast_RAM=ts(exp(forecast_RAM),start=c(2016,2),frequency=12) # make the time series
  forecast_RAM2
  
  y_RAM=c(as.numeric(nights_RAM),as.numeric(forecast_RAM))
  final_RAM=ts(y_RAM,start=c(2006,1),frequency=12)
  plot(final_RAM, col="blue")
  lines(nights_RAM, col="black")
  
  plot(decompose(final_RAM))
  
  mafinal_Alc=ma(final_Alc,order=12)
  mafinal_Alg=ma(final_Alg,order=12)
  mafinal_AML=ma(final_AML,order=12)
  mafinal_Cen=ma(final_Cen,order=12)
  mafinal_Nor=ma(final_Nor,order=12)
  mafinals_RAM=ma(final_RAM,order=12)
  mafinal_RAA=ma(final_RAA,order=12)
  
  ################HOLT WINTERS
  
  
  
  #Holt-Winters additive method(fit1) multiplicative method(fit2)
  
  
  fit_AlcA <- hw(nights_Alc,seasonal="additive")
  fit_AlcM <- hw(nights_Alc,seasonal="multiplicative")
  
  plot(fit_AlcM,ylab="International visitor night in Alc (millions)",
       plot.conf=FALSE, type="o", fcol="white", xlab="Year")
  lines(fitted(fit_AlcA), col="red", lty=2)
  lines(fitted(fit_AlcM), col="green", lty=2)
  lines(fit_AlcA$mean, type="o", col="red")
  lines(fit_AlcM$mean, type="o", col="green")
  legend("topleft",lty=1, pch=1, col=1:3, 
         c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
  manights_Alc=ma(nights_Cen,order=12)
  #Multiplicative seems to be better
  
  forecast_AlcA = data.frame(matrix(unlist(fit_AlcA[2]), nrow=24, byrow=T)) # make the df 
  forecast_AlcA=ts((forecast_AlcA),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_Alc),as.numeric(forecast_AlcA))
  AlcA=ts(y,start=c(2006,1),frequency=12)
  plot(AlcA)
  
  
  forecast_AlcM = data.frame(matrix(unlist(fit_AlcM[2]), nrow=24, byrow=T)) # make the df 
  forecast_AlcM=ts((forecast_AlcM),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_Alc),as.numeric(forecast_AlcM))
  AlcM=ts(y,start=c(2006,1),frequency=12)
  plot(AlcM)
  
  
  
  fit_AlgA <- hw(nights_Alg,seasonal="additive")
  fit_AlgM <- hw(nights_Alg,seasonal="multiplicative")
  
  plot(fit_AlgM,ylab="International visitor night in Alg (millions)",
       plot.conf=FALSE, type="o", fcol="white", xlab="Year")
  lines(fitted(fit_AlgA), col="red", lty=2)
  lines(fitted(fit_AlgM), col="green", lty=2)
  lines(fit_AlgA$mean, type="o", col="red")
  lines(fit_AlgM$mean, type="o", col="green")
  legend("topleft",lty=1, pch=1, col=1:3, 
         c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
  manights_Alg=ma(nights_Alg,order=12)
  #Additive seems to fit better
  forecast_AlgA = data.frame(matrix(unlist(fit_AlgA[2]), nrow=24, byrow=T)) # make the df 
  forecast_AlgA=ts((forecast_AlgA),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_Alg),as.numeric(forecast_AlgA))
  AlgA=ts(y,start=c(2006,1),frequency=12)
  plot(AlgA)
  
  
  forecast_AlgM = data.frame(matrix(unlist(fit_AlgM[2]), nrow=24, byrow=T)) # make the df 
  forecast_AlgM=ts((forecast_AlgM),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_Alg),as.numeric(forecast_AlgM))
  AlgM=ts(y,start=c(2006,1),frequency=12)
  plot(AlgM)
  
  
  fit_AMLA <- hw(nights_AML,seasonal="additive")
  fit_AMLM <- hw(nights_AML,seasonal="multiplicative")
  
  plot(fit_AMLM,ylab="International visitor night in AML (millions)",
       plot.conf=FALSE, type="o", fcol="white", xlab="Year")
  lines(fitted(fit_AMLA), col="red", lty=2)
  lines(fitted(fit_AMLM), col="green", lty=2)
  lines(fit_AMLA$mean, type="o", col="red")
  lines(fit_AMLM$mean, type="o", col="green")
  legend("topleft",lty=1, pch=1, col=1:3, 
         c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
  manights_AML=ma(nights_AML,order=12)
  #Additive seems to fit better
  forecast_AMLA = data.frame(matrix(unlist(fit_AMLA[2]), nrow=24, byrow=T)) # make the df 
  forecast_AMLA=ts((forecast_AMLA),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_AML),as.numeric(forecast_AMLA))
  AMLA=ts(y,start=c(2006,1),frequency=12)
  plot(AMLA)
  
  
  forecast_AMLM = data.frame(matrix(unlist(fit_AMLM[2]), nrow=24, byrow=T)) # make the df 
  forecast_AMLM=ts((forecast_AMLM),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_AML),as.numeric(forecast_AMLM))
  AMLM=ts(y,start=c(2006,1),frequency=12)
  plot(AMLM)
  
  ##########################################
  #In the detrended time series the multiplicative model
  #seems to be quite accurate
  
  
  fit_CenA <- hw(nights_Cen,seasonal="additive")
  fit_CenM <- hw(nights_Cen,seasonal="multiplicative")
  
  plot(fit_CenM,ylab="International visitor night in Centro (millions)",
       plot.conf=FALSE, type="o", fcol="white", xlab="Year")
  lines(fitted(fit_CenA), col="red", lty=2)
  lines(fitted(fit_CenM), col="green", lty=2)
  lines(fit_CenA$mean, type="o", col="red")
  lines(fit_CenM$mean, type="o", col="green")
  legend("topleft",lty=1, pch=1, col=1:3, 
         c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
  manights_Cen=ma(nights_Cen,order=12)
  
  forecast_CenA = data.frame(matrix(unlist(fit_CenA[2]), nrow=24, byrow=T)) # make the df 
  forecast_CenA=ts((forecast_CenA),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_Cen),as.numeric(forecast_CenA))
  CenA=ts(y,start=c(2006,1),frequency=12)
  plot(CenA)
  
  
  forecast_CenM = data.frame(matrix(unlist(fit_CenM[2]), nrow=24, byrow=T)) # make the df 
  forecast_CenM=ts((forecast_CenM),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_Cen),as.numeric(forecast_CenM))
  CenM=ts(y,start=c(2006,1),frequency=12)
  plot(CenM)
  
  
  fit_NorA <- hw(nights_Nor,seasonal="additive")
  fit_NorM <- hw(nights_Nor,seasonal="multiplicative")
  
  plot(fit_NorM,ylab="International visitor night in Nor (millions)",
       plot.conf=FALSE, type="o", fcol="white", xlab="Year")
  lines(fitted(fit_NorA), col="red", lty=2)
  lines(fitted(fit_NorM), col="green", lty=2)
  lines(fit_NorA$mean, type="o", col="red")
  lines(fit_NorM$mean, type="o", col="green")
  legend("topleft",lty=1, pch=1, col=1:3, 
         c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
  manights_Nor=ma(nights_Nor,order=12)
  
  forecast_NorA = data.frame(matrix(unlist(fit_NorA[2]), nrow=24, byrow=T)) # make the df 
  forecast_NorA=ts((forecast_NorA),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_Nor),as.numeric(forecast_NorA))
  NorA=ts(y,start=c(2006,1),frequency=12)
  plot(NorA)
  
  
  forecast_NorM = data.frame(matrix(unlist(fit_NorM[2]), nrow=24, byrow=T)) # make the df 
  forecast_NorM=ts((forecast_NorM),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_Nor),as.numeric(forecast_NorM))
  NorM=ts(y,start=c(2006,1),frequency=12)
  plot(NorM)
  
  fit_RAMA <- hw(nights_RAM,seasonal="additive")
  fit_RAMM <- hw(nights_RAM,seasonal="multiplicative")
  
  plot(fit_RAMM,ylab="International visitor night in Alg (millions)",
       plot.conf=FALSE, type="o", fcol="white", xlab="Year")
  lines(fitted(fit_RAMA), col="red", lty=2)
  lines(fitted(fit_RAMM), col="green", lty=2)
  lines(fit_RAMA$mean, type="o", col="red")
  lines(fit_RAMM$mean, type="o", col="green")
  legend("topleft",lty=1, pch=1, col=1:3, 
         c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
  manights_RAM=ma(nights_RAM,order=12)
  
  forecast_RAMA = data.frame(matrix(unlist(fit_RAMA[2]), nrow=24, byrow=T)) # make the df 
  forecast_RAMA=ts((forecast_RAMA),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_RAM),as.numeric(forecast_RAMA))
  RAMA=ts(y,start=c(2006,1),frequency=12)
  plot(RAMA)
  
  
  forecast_RAMM = data.frame(matrix(unlist(fit_RAMM[2]), nrow=24, byrow=T)) # make the df 
  forecast_RAMM=ts((forecast_RAMM),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_RAM),as.numeric(forecast_RAMM))
  RAMM=ts(y,start=c(2006,1),frequency=12)
  plot(RAMM)
  
  
  fit_RAAA <- hw(nights_RAA,seasonal="additive")
  fit_RAAM <- hw(nights_RAA,seasonal="multiplicative")
  
  plot(fit_RAAM,ylab="International visitor night in Alg (millions)",
       plot.conf=FALSE, type="o", fcol="white", xlab="Year")
  lines(fitted(fit_RAAA), col="red", lty=2)
  lines(fitted(fit_RAAM), col="green", lty=2)
  lines(fit_RAAA$mean, type="o", col="red")
  lines(fit_RAAM$mean, type="o", col="green")
  legend("topleft",lty=1, pch=1, col=1:3, 
         c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
  manights_RAA=ma(nights_RAA,order=12)
  
  
  forecast_RAAA = data.frame(matrix(unlist(fit_RAAA[2]), nrow=24, byrow=T)) # make the df 
  forecast_RAAA=ts((forecast_RAAA),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_RAA),as.numeric(forecast_RAAA))
  RAAA=ts(y,start=c(2006,1),frequency=12)
  plot(RAAA)
  
  
  forecast_RAAM = data.frame(matrix(unlist(fit_RAAM[2]), nrow=24, byrow=T)) # make the df 
  forecast_RAAM=ts((forecast_RAAM),start=c(2016,2),frequency=12) # make the time series
  #merge the two time series
  y=c(as.numeric(nights_RAA),as.numeric(forecast_RAAM))
  RAAM=ts(y,start=c(2006,1),frequency=12)
  plot(RAAM)
