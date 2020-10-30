# Series de Tiempo, Septiembre de 2020
# Clase 9. Aplicacion ARIMA(p, d, q) y ARMA(p, q)
#****************************************************************************************
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages(stats)
install.packages("forecast")
install.packages("lubridate")
# 
library(ggplot2)
library(dplyr)
library(readxl)
library(stats)
library(lubridate)
# 
#****************************************************************************************
#setwd("Series-de-Tiempo-Fall2020/Clase_10.5")
getwd()

#****************************************************************************************
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
# Funciones que extrae y grafica las raices del polinomio caracteristico

source("arroots.R")
source("maroots.R")
source("plot.armaroots.R")

#****************************************************************************************
# Conversion a series de tiempo:
# 

Pax_Nal <- ts(Datos$Pax_Nal, 
              start = c(2000, 1), 
              freq = 12)

LPax_Nal <- ts(log(Datos$Pax_Nal), 
               start = c(2000, 1), 
               freq = 12)

DLPax_Nal <- ts(log(Datos$Pax_Nal) - lag(log(Datos$Pax_Nal), k = 1),
                start = c(2000, 1), 
                freq = 12)

DLPax_Nal_S <- diff(DLPax_Nal,12)

D_Ene <- ts(Datos$D_Ene, 
            start = c(2000, 1), 
            freq = 12)

D_Feb <- ts(Datos$D_Feb, 
            start = c(2000, 1), 
            freq = 12)

D_Jul <- ts(Datos$D_Jul, 
            start = c(2000, 1), 
            freq = 12)

D_Dic <- ts(Datos$D_Dic, 
            start = c(2000, 1), 
            freq = 12)

D_May2009 <- ts(Datos$D_May2009, 
                start = c(2000, 1), 
                freq = 12)

D_Jun2009 <- ts(Datos$D_Jun2009, 
                start = c(2000, 1), 
                freq = 12)

#****************************************************************************************
# Graficas:

par(mfrow=c(3,1))

plot(Pax_Nal, xlab = "Tiempo", ylab = "Pasajeros",
     main = "Pasajeros en vuelos nacionales de salida",
     col = "darkgreen")

plot(LPax_Nal, xlab = "Tiempo", ylab = "LN Pasajeros", 
     main = "LN Pasajeros en vuelos nacionales de salida",
     col = "darkblue")

plot(DLPax_Nal, xlab = "Tiempo",  ylab = "DLN Pasajeros",
     main = "Diff LN Pasajeros en vuelos nacionales de salida", 
     col = "darkred")

par(mfrow=c(1,1))

# Visualizar estacionalidad

boxplot(DLPax_Nal~month(Datos$Periodo), 
        xlab= "Mes", main = "Crecimiento mensual respecto al mes anterior")
points(DLPax_Nal~month(Datos$Periodo), col="blue")
abline(h=0, col="red")

boxplot(DLPax_Nal_S~month(Datos$Periodo[-c(1:12)]))
points(DLPax_Nal_S~month(Datos$Periodo)[-c(1:12)], col="blue")
abline(h=0, col="red")

par(mfrow=c(2,1))

plot(DLPax_Nal, xlab = "Tiempo", ylab = "LN Pasajeros", 
     main = "LN Pasajeros en vuelos nacionales de salida",
     col = "darkblue")


plot(diff(DLPax_Nal,12), xlab = "Tiempo",  ylab = "SDLN Pasajeros",
     main = "Seasonal Diff LN Pasajeros en vuelos nacionales de salida", 
     col = "darkred")

par(mfrow=c(1,1))


#****************************************************************************************
# Funciones que determinan el rezago optimo (que minimiza el criterio de Akaike)
# Esta es una prueba mas formar respecto de la meta inspeccion grafica

source("Lag_Opt_ARIMA_Exog.R")
source("Lag_Opt_SARIMA_Exog.R")
# La funcion necesita de parametros definidos como:
# Lag_Opt_ARIMA_Ex(p_max, d_max, q_max, X_t, Ex, Z_t){
# p_max: Rezagos maximos a evaluar del componente AR
# q_max: Rezagos maximos a evaluar del componente MA
# P_max: Rezagos maximos a evaluar del componente ARm estacional
# Q_max: Rezagos maximos a evaluar del componente MAm estacional 
# d_max: Numero de diferencias evualadas
# X_t: Serie de Tiempo modelada
# Z_t: Vector o Matriz de variables exogenas
# Ex: es indicador de que incluye variables exogenas (0 indica que no se incluyen, y 1 que si)


#############################################################
# ARIMA Optimo

dumm_ARIMA = cbind(D_Ene,D_Feb, D_Jul, D_Dic,D_May2009, D_Jun2009)
dumm_SARIMA <-cbind(D_May2009[-c(1:12)], D_Jun2009[-c(1:12)])


Lag_Opt_ARIMA_Exog(p_max = 6, q_max = 6, 
                   X_t = DLPax_Nal, 
                   Ex = 1,
                   Z_t = dumm_ARIMA)

ARIMA_Ex_DLPax_Nal <- arima(DLPax_Nal, c(4,0,6), 
                            xreg=dumm_ARIMA,
                            method = "ML")

ARIMA_Ex_DLPax_Nal

par(mfrow=c(1,2))
plot.armaroots(arroots(ARIMA_Ex_DLPax_Nal), 
               main="Inverse AR roots of \nAR(p): LN PAx Nal")
plot.armaroots(maroots(ARIMA_Ex_DLPax_Nal), 
               main="Inverse AR roots of \nAR(p): LN PAx Nal")
par(mfrow=c(1,1))
acf(ARIMA_Ex_DLPax_Nal$residuals, na.action = na.pass)

plot(ARIMA_Ex_DLPax_Nal$residuals)

###########################################################
# SARIMA optimo

AIC_SARIMA = Lag_Opt_SARIMA_Exog(p_max = 8, q_max = 8,
                    P_max = 0, Q_max = 1,
                   X_t = DLPax_Nal_S,
                   Ex = 0)

AIC_SARIMA_D = Lag_Opt_SARIMA_Exog(p_max = 5, q_max = 5, 
                    P_max = 0, Q_max = 2,
                    X_t = DLPax_Nal_S, 
                    Ex = 1,
                    Z_t = dumm_SARIMA)



SARIMA_Ex_DLPax_Nal <- arima(DLPax_Nal_S, order = c(4, 0, 4),
                             seasonal = c(1,0,1),
                           xreg = dumm_SARIMA,
                           method = "ML")

SARIMA_Ex_DLPax_Nal

par(mfrow=c(1,2))
plot.armaroots(arroots(SARIMA_Ex_DLPax_Nal), 
               main="Inverse AR roots of \nAR(p): LN PAx Nal")
plot.armaroots(maroots(SARIMA_Ex_DLPax_Nal), 
               main="Inverse AR roots of \nAR(p): LN PAx Nal")
par(mfrow=c(1,1))

acf(SARIMA_Ex_DLPax_Nal$residuals, na.action = na.pass)

plot(SARIMA_Ex_DLPax_Nal$residuals)

# libreria forecast

# ARIMA auto.arima()

library(forecast)

?auto.arima
auto.arima

ARIMA_model = auto.arima(DLPax_Nal,xreg=dumm_ARIMA, seasonal = FALSE)
ARIMA_model
plot(ARIMA_model)
autoplot(ARIMA_model)
checkresiduals(ARIMA_model)



SARIMA_model = auto.arima(DLPax_Nal_S, xreg =dumm_SARIMA)
SARIMA_model
plot(SARIMA_model)

dumm_SARIMA = cbind(D_May2009, D_Jun2009)

SARIMA_model2= auto.arima(LPax_Nal,xreg = dumm_SARIMA[,1])
SARIMA_model2
autoplot(SARIMA_model2)

checkresiduals(SARIMA_model2)
plot(SARIMA_model2$residuals)


plot(ARIMA_model$residuals, col = "blue")
lines(SARIMA_model2$residuals)

plot(SARIMA_model$residuals)
lines(SARIMA_model2$residuals, col='red')




SARIMA_model2_f = forecast(SARIMA_model2, xreg = cbind(rep(0,36)))

SARIMA_model2_f$mean = exp(SARIMA_model2_f$mean)
SARIMA_model2_f$upper = exp(SARIMA_model2_f$upper)
SARIMA_model2_f$lower = exp(SARIMA_model2_f$lower)
SARIMA_model2_f$x = exp(SARIMA_model2_f$x)
SARIMA_model2_f$fitted = exp(SARIMA_model2_f$fitted)


plot(SARIMA_model2_f)

autoplot(SARIMA_model2_f)+
        scale_y_continuous(labels = scales::comma)



# Comparando residuales 

plot(ARIMA_model$residuals, col="red")
lines(SARIMA_model2$residuals)

plot(SARIMA_Ex_DLPax_Nal$residuals)
lines(SARIMA_model2$residuals, col = "red")
