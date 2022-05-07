library(readxl)
QualidadeARO3 <- read_excel("C:/Users/bruno/Downloads/QualidadeARO3.xlsx")
View(QualidadeARO3)
QualidadeARO3<-as.data.frame(QualidadeARO3)

# 8784 obs in 366 days => 24 obs per day

# MA(q): PACF -> 0, ACF(k)=0, k>q
# AR(p): ACF -> 0, PACF(k)=0, k>p
# ARMA(p,q): ACF -> 0, PACF -> 0

library(imputeTS)
library(extremogram)
library(tseries)
library(forecast)
library(sarima)
library(zoo)
########## ANTAS-ESPINHO ##########

#experiencias
class(QualidadeARO3)
model=auto.arima(QualidadeARO3[,1], D=1)
forecast = forecast(model, h = 10)
plot(forecast)
start(QualidadeARO3); end(QualidadeARO3); frequency(QualidadeARO3)
x <- sarima:::rgarch1p1(ar(2), alpha = 0.3, beta = 0.55, omega = 1, n.skip = 100)

# Missing values
statsNA(QualidadeARO3[,1])
ggplot_na_distribution(QualidadeARO3[,1])

# Time series decomposition

AntasEspinho_TS=ts(QualidadeARO3[,1], start=c(1, 1), end=c(366, 24), frequency=24)
isStationaryModel()
plot(AntasEspinho_TS)
boxplot(AntasEspinho_TS ~ cycle(AntasEspinho_TS))
AntasEspinho_TSmonthly<-aggregate(AntasEspinho_TS)
AntasEspinho_TSannual<-aggregate(AntasEspinho_TS)/12
layout(1:2)
plot(aggregate(AntasEspinho_TSmonthly))
plot(AntasEspinho_TSannual)
dec.Antas<-(decompose(AntasEspinho_TS))

ts.plot(cbind(dec.Antas$trend, dec.Antas$trend * dec.Antas$seasonal), lty = 1:2)
Antas.195 <- window(AntasEspinho_TSmonthly, start = c(1,100), freq = TRUE)
Antas.265 <- window(AntasEspinho_TSmonthly, start = c(1,261), freq = TRUE)
A195.ratio <- mean(Antas.195) / mean(AntasEspinho_TSmonthly)
A265.ratio <- mean(Antas.265) / mean(AntasEspinho_TSmonthly)

#centred moving average

autoplot(AntasEspinho_TS, series="Data") +
  autolayer(ma(AntasEspinho_TS,5), series="5-MA") 
autoplot(AntasEspinho_TS, series="Data") +
  autolayer(ma(AntasEspinho_TS,20), series="20-MA")
autoplot(AntasEspinho_TS, series="Data") +
  autolayer(ma(AntasEspinho_TS,10), series="10-MA")
####

AntasEspinho_STL=stl(AntasEspinho_TS,s.window='periodic')
plot(AntasEspinho_STL)
# 15-day seasonality?

# ACF and PACF
AntasEspinho_ACF<-acf(AntasEspinho_TS, lag.max=300, main='Wismar')
AntasEspinho_PACF<-pacf(AntasEspinho_TS, lag.max=300, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(10)?

# Residuals
AntasEspinho_res=AntasEspinho_STL$time.series[,3]
plot(AntasEspinho_res)
AntasEspinho_res_ACF<-acf(AntasEspinho_res, lag.max=100, main='Residuals-Wismar')
AntasEspinho_res_PACF<-pacf(AntasEspinho_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
AntasEspinho_Extremogram<-extremogram1(AntasEspinho_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# library(fUnitRoots)
# urkpssTest(AntasEspinho_TS, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
# tsstationary = diff(AntasEspinho_TS, differences=1)
# plot(tsstationary)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(AntasEspinho_TS) # STATIONARY

# Yule-Walker estimation
ar.yw(AntasEspinho_TS, order.max = 10) # AR(4)

# Auto ARIMA

model=auto.arima(QualidadeARO3[,1], D=1, Q=2) #D is the seasonal difference order
forecast = forecast(model, h = 10)

########## ENTRECAMPOS ##########

# Missing values
statsNA(QualidadeARO3[,2])
ggplot_na_distribution(QualidadeARO3[,2])

# Time series decomposition
Entrecampos_TS=ts(QualidadeARO3[,2], start=c(1, 1), end=c(366, 24), frequency=24)
Entrecampos_STL=stl(Entrecampos_TS,s.window='periodic')
plot(Entrecampos_STL)
# 15-day seasonality?

# ACF and PACF
Entrecampos_ACF<-acf(Entrecampos_TS, lag.max=300, main='Wismar')
Entrecampos_PACF<-pacf(Entrecampos_TS, lag.max=300, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(8)?

# Residuals
Entrecampos_res=Entrecampos_STL$time.series[,3]
plot(Entrecampos_res)
Entrecampos_res_ACF<-acf(Entrecampos_res, lag.max=100, main='Residuals-Wismar')
Entrecampos_res_PACF<-pacf(Entrecampos_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
Entrecampos_Extremogram<-extremogram1(Entrecampos_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(Entrecampos_TS) #STATIONARY

# Yule-Walker estimation
ar.yw(Entrecampos_TS, order.max = 100) # AR(98)

# Auto ARIMA
auto.arima(Entrecampos_TS)

########## ESTARREJA ##########

# Missing values
statsNA(QualidadeARO3[,3])
ggplot_na_distribution(QualidadeARO3[,3])

# Time series decomposition
Estarreja_TS=ts(QualidadeARO3[,3], start=c(1, 1), end=c(366, 24), frequency=24)
Estarreja_STL=stl(Estarreja_TS,s.window='periodic')
plot(Estarreja_STL)
# 15-day seasonality?

# ACF and PACF
Estarreja_ACF<-acf(Estarreja_TS, lag.max=300, main='Wismar')
Estarreja_PACF<-pacf(Estarreja_TS, lag.max=300, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(12)?

# Residuals
Estarreja_res=Estarreja_STL$time.series[,3]
plot(Estarreja_res)
Estarreja_res_ACF<-acf(Estarreja_res, lag.max=100, main='Residuals-Wismar')
Estarreja_res_PACF<-pacf(Estarreja_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
Estarreja_Extremogram<-extremogram1(Estarreja_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(Estarreja_TS) # STATIONARY

# Yule-Walker estimation
ar.yw(Estarreja_TS, order.max = 10) # AR(8)

# Auto ARIMA
auto.arima(Estarreja_TS)

########## ÍLHAVO ##########

# Missing values
statsNA(QualidadeARO3[,4])
ggplot_na_distribution(QualidadeARO3[,4])

# Time series decomposition
Ilhavo_TS=ts(QualidadeARO3[,4], start=c(1, 1), end=c(366, 24), frequency=24)
Ilhavo_STL=stl(Ilhavo_TS,s.window='periodic')
plot(Ilhavo_STL)
# 15-day seasonality?

# ACF and PACF
Ilhavo_ACF<-acf(Ilhavo_TS, lag.max=300, main='Wismar')
Ilhavo_PACF<-pacf(Ilhavo_TS, lag.max=300, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(11)?

# Residuals
Ilhavo_res=Ilhavo_STL$time.series[,3]
plot(Ilhavo_res)
Ilhavo_res_ACF<-acf(Ilhavo_res, lag.max=100, main='Residuals-Wismar')
Ilhavo_res_PACF<-pacf(Ilhavo_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
Ilhavo_Extremogram<-extremogram1(Ilhavo_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(Ilhavo_TS) # STATIONARY

# Yule-Walker estimation
ar.yw(Ilhavo_TS, order.max = 100) # AR(98)

# Auto ARIMA
auto.arima(Ilhavo_TS)

########## LARANJEIRO-ALMADA ##########

# Missing values
statsNA(QualidadeARO3[,5])
ggplot_na_distribution(QualidadeARO3[,5])

# Time series decomposition
LaranjeiroAlmada_TS=ts(QualidadeARO3[,5], start=c(1, 1), end=c(366, 24), frequency=24)
LaranjeiroAlmada_STL=stl(LaranjeiroAlmada_TS,s.window='periodic')
plot(LaranjeiroAlmada_STL)
# 15-day seasonality?

# ACF and PACF
LaranjeiroAlmada_ACF<-acf(LaranjeiroAlmada_TS, lag.max=300, main='Wismar')
LaranjeiroAlmada_PACF<-pacf(LaranjeiroAlmada_TS, lag.max=300, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(14)?

# Residuals
LaranjeiroAlmada_res=LaranjeiroAlmada_STL$time.series[,3]
plot(LaranjeiroAlmada_res)
LaranjeiroAlmada_res_ACF<-acf(LaranjeiroAlmada_res, lag.max=100, main='Residuals-Wismar')
LaranjeiroAlmada_res_PACF<-pacf(LaranjeiroAlmada_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
LaranjeiroAlmada_Extremogram<-extremogram1(LaranjeiroAlmada_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(LaranjeiroAlmada_TS) # STATIONARY

# Yule-Walker estimation
ar.yw(LaranjeiroAlmada_TS, order.max = 100) # AR(98)

# Auto ARIMA
auto.arima(LaranjeiroAlmada_TS)

########## MEM-MARTINS ##########

# Missing values
statsNA(QualidadeARO3[,6])
ggplot_na_distribution(QualidadeARO3[,6])

# Time series decomposition
MemMartins_TS=ts(QualidadeARO3[,6], start=c(1, 1), end=c(366, 24), frequency=24)
MemMartins_STL=stl(MemMartins_TS,s.window='periodic')
plot(MemMartins_STL)
# 15-day seasonality?

# ACF and PACF
MemMartins_ACF<-acf(MemMartins_TS, lag.max=300, main='Wismar')
MemMartins_PACF<-pacf(MemMartins_TS, lag.max=300, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(9)?

# Residuals
MemMartins_res=MemMartins_STL$time.series[,3]
plot(MemMartins_res)
MemMartins_res_ACF<-acf(MemMartins_res, lag.max=100, main='Residuals-Wismar')
MemMartins_res_PACF<-pacf(MemMartins_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
MemMartins_Extremogram<-extremogram1(MemMartins_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(MemMartins_TS) # STATIONARY

# Yule-Walker estimation
ar.yw(MemMartins_TS, order.max = 10) # AR(9)

# Auto ARIMA
auto.arima(MemMartins_TS)

########## PAIO-PIRES ##########

# Missing values
statsNA(QualidadeARO3[,7])
ggplot_na_distribution(QualidadeARO3[,7])

# Time series decomposition
PaioPires_TS=ts(QualidadeARO3[,7], start=c(1, 1), end=c(366, 24), frequency=24)
PaioPires_STL=stl(PaioPires_TS,s.window='periodic')
plot(PaioPires_STL)
# 15-day seasonality?

# ACF and PACF
PaioPires_ACF<-acf(PaioPires_TS, lag.max=300, main='Wismar')
PaioPires_PACF<-pacf(PaioPires_TS, lag.max=750, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(29)?

# Residuals
PaioPires_res=PaioPires_STL$time.series[,3]
plot(PaioPires_res)
PaioPires_res_ACF<-acf(PaioPires_res, lag.max=100, main='Residuals-Wismar')
PaioPires_res_PACF<-pacf(PaioPires_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
PaioPires_Extremogram<-extremogram1(PaioPires_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(PaioPires_TS) # STATIONARY

# Yule-Walker estimation
ar.yw(PaioPires_TS, order.max = 10) # AR(9)

# Auto ARIMA
auto.arima(PaioPires_TS)

########## RESTELO ##########

# Missing values
statsNA(QualidadeARO3[,8])
ggplot_na_distribution(QualidadeARO3[,8])

# Time series decomposition
Restelo_TS=ts(QualidadeARO3[,8], start=c(1, 1), end=c(366, 24), frequency=24)
Restelo_STL=stl(Restelo_TS,s.window='periodic')
plot(Restelo_STL)
# 15-day seasonality?

# ACF and PACF
Restelo_ACF<-acf(Restelo_TS, lag.max=300, main='Wismar')
Restelo_PACF<-pacf(Restelo_TS, lag.max=300, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(10)?

# Residuals
Restelo_res=Restelo_STL$time.series[,3]
plot(Restelo_res)
Restelo_res_ACF<-acf(Restelo_res, lag.max=100, main='Residuals-Wismar')
Restelo_res_PACF<-pacf(Restelo_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
Restelo_Extremogram<-extremogram1(Restelo_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(Restelo_TS) # STATIONARY

# Yule-Walker estimation
ar.yw(Restelo_TS, order.max = 10) # AR(9)

# Auto ARIMA
auto.arima(Restelo_TS)

########## SOBREIRAS-PORTO ##########

# Missing values
statsNA(QualidadeARO3[,9])
ggplot_na_distribution(QualidadeARO3[,9])

# Time series decomposition
SobreirasPorto_TS=ts(QualidadeARO3[,9], start=c(1, 1), end=c(366, 24), frequency=24)
SobreirasPorto_STL=stl(SobreirasPorto_TS,s.window='periodic')
plot(SobreirasPorto_STL)
# 15-day seasonality?

# ACF and PACF
SobreirasPorto_ACF<-acf(SobreirasPorto_TS, lag.max=300, main='Wismar')
SobreirasPorto_PACF<-pacf(SobreirasPorto_TS, lag.max=500, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(14)?

# Residuals
SobreirasPorto_res=SobreirasPorto_STL$time.series[,3]
plot(SobreirasPorto_res)
SobreirasPorto_res_ACF<-acf(SobreirasPorto_res, lag.max=100, main='Residuals-Wismar')
SobreirasPorto_res_PACF<-pacf(SobreirasPorto_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
SobreirasPorto_Extremogram<-extremogram1(SobreirasPorto_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(SobreirasPorto_TS) # STATIONARY

# Yule-Walker estimation
ar.yw(SobreirasPorto_TS, order.max = 100) # AR(98)

# Auto ARIMA
auto.arima(SobreirasPorto_TS)

########## VILHA NOVA DA TELHA-MAIA ##########

# Missing values
statsNA(QualidadeARO3[,10])
ggplot_na_distribution(QualidadeARO3[,10])

# Time series decomposition
VNTelhaMaia_TS=ts(QualidadeARO3[,10], start=c(1, 1), end=c(366, 24), frequency=24)
VNTelhaMaia_STL=stl(VNTelhaMaia_TS,s.window='periodic')
plot(VNTelhaMaia_STL)
# 15-day seasonality?

# ACF and PACF
VNTelhaMaia_ACF<-acf(VNTelhaMaia_TS, lag.max=300, main='Wismar')
VNTelhaMaia_PACF<-pacf(VNTelhaMaia_TS, lag.max=500, main='Wismar')
# AR(1), AR(2), AR(3), ... AR(14)?

# Residuals
VNTelhaMaia_res=VNTelhaMaia_STL$time.series[,3]
plot(VNTelhaMaia_res)
VNTelhaMaia_res_ACF<-acf(VNTelhaMaia_res, lag.max=100, main='Residuals-Wismar')
VNTelhaMaia_res_PACF<-pacf(VNTelhaMaia_res, lag.max=100, main='Residuals-Wismar')

# Extremogram
VNTelhaMaia_Extremogram<-extremogram1(VNTelhaMaia_TS, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

# Augmented Dickey-Fuller Test -> H0: non-stationary
adf.test(VNTelhaMaia_TS) # STATIONARY

# Yule-Walker estimation
ar.yw(VNTelhaMaia_TS, order.max = 100) # AR(98)

# Auto ARIMA
auto.arima(VNTelhaMaia_TS)
?auto.arima
