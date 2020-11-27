# Series de Tiempo, Septiembre de 2020
# Clase 19. Aplicacion ARCH(q)
#****************************************************************************************
#install.packages("dynlm")
#install.packages("moments")
#install.packages("broom")
#install.packages("FinTS")
#install.packages('rugarch')
#install.packages("tsbox")
#install.packages("metRology")
#install.packages("expm")
library(expm)
library(Matrix)
library(ggplot2)
library(quantmod)
library(moments)
library(dynlm)
library(broom)
library(FinTS)
library(lubridate)
library(forecast)
library(readxl)
library(MASS)
library(rugarch)
library(tsbox)
## ARCH proceso ARIMA

Datos <- read_excel("Base_Transporte_ARIMA.xlsx", sheet = "Datos", col_names = TRUE)

#****************************************************************************************
# Los datos importados son:
#
# Pax_Nal: Pasajeros (Salidas) en vuelos nacionales
# Pax_Int: Pasajeros (Salidas) en vuelos internacionales 
# Vue_Nal: Vuelos u operaciones (Salidas) en vuelos nacionales
# Vue_Int: Vuelos u operaciones (Salidas) en vuelos internacionales
# Pax_Metro: Pasajeros transportados (Millones) en el SCM
# D_Sep2017: Dummy que toma el valor de 1 en septiembre de 2017
# D_Ene: Dummy que toma el valor de 1 en todos los eneros
# D_Feb: Dummy que toma el valor de 1 en todos los febreros
# D_Jul: Dummy que toma el valor de 1 en todos los julios
# D_Dic: Dummy que toma el valor de 1 en todos los diciembres
# D_May2009: Dummy que toma el valor de 1 en mayo de 2009
# D_Jun2009:Dummy que toma el valor de 1 en junio de 2009
#
# Fuente: INEGI, https://www.inegi.org.mx/app/indicadores/?tm=0&t=1090
#****************************************************************************************

#****************************************************************************************
# Conversion a series de tiempo:
# 

Pax_Nal <- ts(Datos$Pax_Nal, 
              start = c(2000, 1), 
              freq = 12)

LPax_Nal <- ts(log(Datos$Pax_Nal), 
               start = c(2000, 1), 
               freq = 12)

D_May2009 <- ts(Datos$D_May2009, 
                start = c(2000, 1), 
                freq = 12)

D_Jun2009 <- ts(Datos$D_Jun2009, 
                start = c(2000, 1), 
                freq = 12)

#****************************************************************************************

dumm_SARIMA = cbind(D_May2009, D_Jun2009)

ARIMA_model = auto.arima(LPax_Nal,xreg=dumm_SARIMA)

summary(ARIMA_model)

plot(forecast(ARIMA_model, xreg = dumm_SARIMA[1:36,]))


## ACF residuals

checkresiduals(ARIMA_model)

plot(ARIMA_model$residuals^2)

acf(ARIMA_model$residuals^2)

## Estimación de ARCH(1)

ehatsq <- ARIMA_model$residuals^2

ARCH_1 = dynlm(ehatsq~L(ehatsq))

summary(ARCH_1)

T_capital <- length(ehatsq)
q <- length(coef(ARCH_1))-1
Rsq <- glance(ARCH_1)[[1]]
LM <- (T_capital-q)*Rsq
alpha <- 0.05
Chicr <- qchisq(1-alpha, q)
Chicr
LM

ArchTest(ARIMA_model$residuals, lags = 1)

## Obtener datos de las acciones Amazon mediante la libreria 'quantmod'

library(quantmod)

options("getSymbols.warning4.0"=FALSE)

AMZN <-getSymbols("AMZN", src = "yahoo", auto.assign = FALSE)

AMZN <- na.omit(AMZN)

plot(AMZN$AMZN.Adjusted)

logret <- ts(diff(log(AMZN$AMZN.Adjusted))[-1])

plot(logret)

## Calcular VaR 

alpha <- 0.05
VaR <- quantile(logret, alpha)

round(VaR, 4)

qplot(logret , geom = 'histogram') + geom_histogram(fill = 'lightblue' , bins = 30) +
  geom_histogram(aes(logret[logret < quantile(logret , 0.05)]) , fill = 'red' , bins = 30) +
  labs(x = 'Daily Returns')

#Pruebas de normalidad
vector_ret <- as.vector(logret)

##Kurtosis
round(kurtosis(vector_ret),2)
##Sesgo
round(skewness(vector_ret),2)
##Prueba de normalidad 
jarque.test(vector_ret)


# Calcular VaR para una distribución t
t.fit <-fitdistr(vector_ret, "t")
round(t.fit$estimate, 6)

library(metRology)
alpha <- 0.05
set.seed(1289)
rvec <- rt.scaled(length(logret), mean=t.fit$estimate[1],
                  sd = t.fit$estimate[2],
                  df = t.fit$estimate[3])

VaR <- quantile(rvec, alpha)
round(VaR, 6)


## Calcular ARCH

acf(logret)
acf(logret^2)

logret_random <- sample(as.vector(logret),size =  length(logret), replace = FALSE)

acf(logret_random^2)

logret_mean = dynlm(logret~1)

summary(logret_mean)

ehatsq = ts(resid(logret_mean)^2)

ARCH_m = dynlm(ehatsq~L(ehatsq)+L(L(ehatsq)))

summary(ARCH_m)


acf(ARCH_m$residuals)
acf(abs(ARCH_m$residuals))

ArchTest(logret, lags = 1, demean = TRUE)


## estimación ARCH con ugarch library

library(rugarch)

model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 0)), 
                        mean.model = list(armaOrder = c(0,0)), distribution.model = "std")

model.fit = ugarchfit(spec = model.spec , data = logret, solver = 'solnp')


boot.garch <- ugarchboot(model.fit,
                         method = "Partial",
                         sampling = "raw",  #bootstrap from fitted varepsilon
                         n.ahead = 1,          #simulation horizon
                         n.bootpred = 100000, #number of simulations 
                         solver = "solnp")

## VaR estimado mediante ARCH
rvec <- boot.garch@fseries
alpha <- 0.05
VaR <- quantile(rvec, alpha)
round(VaR,6)

F_inv = qt(0.05, 2.9079)

tiempo= time(AMZN[-1])

ggplot()+
  geom_point(aes(y = logret,x = tiempo),colour = 'lightgrey' , size = 2) + 
  geom_line(aes(y = model.fit@fit$sigma*(F_inv), x = tiempo), colour = 'red') +
  geom_hline(yintercept = sd(logret)*qnorm(0.05) , colour = 'darkgreen' , size = 1.2) + theme_light() + 
  labs(x = '' , y = 'Daily Returns' , title = 'Value at Risk Comparison')

