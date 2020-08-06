# Series de Tiempo, Septiembre de 2020
# Clase 8. Aplicacion MA(q)
#****************************************************************************************
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages(stats)
# 
library(ggplot2)
library(dplyr)
library(readxl)
library(stats)
# 
#****************************************************************************************
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_8")
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
# D_Dic: Dummy que toma el valor de 1 en todos los diciembres
#
# Fuente: INEGI, https://www.inegi.org.mx/app/indicadores/?tm=0&t=1090
#****************************************************************************************
# Funciones que extrae y grafica las raices del polinomio caracteristico

source("maroots.R")
source("plot.armaroots.R")

#****************************************************************************************
# Conversion a series de tiempo:
# 

Pax_Metro <- ts(Datos$Pax_Metro, 
                start = c(2000, 1), 
                freq = 12)

LPax_Metro <- ts(log(Datos$Pax_Metro), 
                 start = c(2000, 1), 
                 freq = 12)

DLPax_Metro <- ts(log(Datos$Pax_Metro) - lag(log(Datos$Pax_Metro), k = 1),
                  start = c(2000, 1), 
                  freq = 12)

D_Sep2017 <- ts(Datos$D_Sep2017, 
                start = c(2000, 1), 
                freq = 12)

D_Oct2017 <- ts(Datos$D_Oct2017, 
                start = c(2000, 1), 
                freq = 12)

#****************************************************************************************
# Graficas:

par(mfrow=c(3,1))

plot(Pax_Metro, xlab = "Tiempo", ylab = "Pasajeros",
     main = "Pasajeros transportados (Millones) en el SCM",
     col = "darkgreen")

plot(LPax_Metro, xlab = "Tiempo", ylab = "LN Pasajeros",
     main = "LN Pasajeros transportados (Millones) en el SCM",
     col = "darkblue")

plot(DLPax_Metro, xlab = "Tiempo", ylab = "DLN Pasajeros",
     main = "Diff LN Pasajeros transportados (Millones) en el SCM", 
     col = "darkred")

par(mfrow=c(1,1))

#****************************************************************************************
# Estimacion del proceo MA(q):

arima(LPax_Metro, order = c(0, 0, 1), 
      xreg = D_Sep2017,
      method = "ML")

#****************************************************************************************
# MA(q) en NIVELES:

pacf(LPax_Metro[1:234], lag.max = 14, 
     xlab = 'Rezagos en k meses')

# 1:
MA_LPax_Metro <- arima(LPax_Metro, order = c(0, 0, 4), 
                       method = "ML")

MA_LPax_Metro

plot(MA_LPax_Metro$residuals,
     main = "Residuales de un MA (4) de LN Pasajeros transportados (Millones) en el SCM",
     ylab = "Residuals MA(q)", xlab = "Tiempo",
     col = "darkred")

plot.armaroots(maroots(MA_LPax_Metro), 
               main="Inverse MA roots of \nMA(p): LN PAx Metro")

# 2:
MA_LPax_Metro_2 <- arima(LPax_Metro, order = c(0, 0, 4),
                       xreg = D_Sep2017,
                       method = "ML")

MA_LPax_Metro_2

plot(MA_LPax_Metro_2$residuals,
     main = "Residuales de un MA (4) de LN Pasajeros transportados (Millones) en el SCM",
     ylab = "Residuals MA(q)", xlab = "Tiempo",
     col = "darkred")

plot.armaroots(maroots(MA_LPax_Metro_2), 
               main="Inverse MA roots of \nMA(p): LN PAx Metro con Dummy")

# Comparacion:

par(mfrow=c(1,2))

plot(MA_LPax_Metro$residuals,
     main = "Sin Dummy",
     ylab = "Residuals MA(q)")

plot(MA_LPax_Metro_2$residuals,
     main = "Con Dummy",
     ylab = "Residuals MA(q) (2)")

par(mfrow=c(1,1))

#

par(mfrow=c(1,2))

plot.armaroots(maroots(MA_LPax_Metro), 
               main="Inverse MA roots of \nMA(p): LN PAx Metro")

plot.armaroots(maroots(MA_LPax_Metro_2), 
               main="Inverse MA roots of \nMA(p): LN PAx Metro con Dummy")

par(mfrow=c(1,1))

#****************************************************************************************
# MA(q) en DIFERENCIAS:

pacf(DLPax_Metro[2:234], lag.max = 14, 
     xlab = 'Rezagos en k meses')

# 1:

MA_DLPax_Metro <- arima(DLPax_Metro, order = c(0, 0, 4),
                        method = "ML")

MA_DLPax_Metro

plot(MA_DLPax_Metro$residuals, col = "darkred",
     main = "Residuales de un MA (4) de DLN Pasajeros transportados (Millones) en el SCM",
     ylab = "Residuals MA(q)")

plot.armaroots(maroots(MA_DLPax_Metro), 
               main="Inverse MA roots of \nMA(p): Diff LN PAx Metro")

# 2:

MA_DLPax_Metro_2 <- arima(DLPax_Metro, order = c(0, 0, 4), 
                        xreg = D_Sep2017,
                        method = "ML")

MA_DLPax_Metro_2

plot(MA_DLPax_Metro_2$residuals, col = "darkred",
     main = "Residuales de un MA (4) de DLN Pasajeros transportados (Millones) en el SCM",
     ylab = "Residuals MA(q)")

plot.armaroots(maroots(MA_DLPax_Metro_2), 
               main="Inverse MA roots of \nMA(p): Diff LN PAx Metro con Dummy")

# Comparacion:

par(mfrow=c(1,2))

plot(MA_DLPax_Metro$residuals,
     main = "Sin Dummy",
     ylab = "Residuals MA(q)")

plot(MA_DLPax_Metro_2$residuals,
     main = "Con Dummy",
     ylab = "Residuals MA(q) (2)")

par(mfrow=c(1,1))

#

par(mfrow=c(1,2))

plot.armaroots(maroots(MA_DLPax_Metro), 
               main="Inverse MA roots of \nMA(p): LN PAx Metro")

plot.armaroots(maroots(MA_DLPax_Metro_2), 
               main="Inverse MA roots of \nMA(p): LN PAx Metro (2)")

par(mfrow=c(1,1))

# 3:

MA_DLPax_Metro_3 <- arima(DLPax_Metro, order = c(0, 0, 4), 
                          xreg = cbind(D_Sep2017, D_Oct2017),
                          method = "ML")

MA_DLPax_Metro_3

plot(MA_DLPax_Metro_3$residuals, col = "darkred",
     main = "Residuales de un MA (4) de DLN Pasajeros transportados (Millones) en el SCM",
     ylab = "Residuals MA(q)")

plot.armaroots(maroots(MA_DLPax_Metro_3), 
               main="Inverse MA roots of \nMA(p): Diff LN PAx Metro")

# Comparacion:

par(mfrow=c(1,2))

plot(MA_DLPax_Metro_2$residuals,
     main = "Sin Dummy",
     ylab = "Residuals MA(q)")

plot(MA_DLPax_Metro_3$residuals,
     main = "Con Dummy",
     ylab = "Residuals MA(q) (2)")

par(mfrow=c(1,1))

#

par(mfrow=c(1,2))

plot.armaroots(maroots(MA_DLPax_Metro_2), 
               main="Inverse MA roots of \nMA(p): LN PAx Metro (2)")

plot.armaroots(maroots(MA_DLPax_Metro_3), 
               main="Inverse MA roots of \nMA(p): LN PAx Metro (3)")

par(mfrow=c(1,1))

#****************************************************************************************
# Estimacion del proceo ARIMA(p, d, q):

arima(LPax_Metro, order = c(1, 0, 1), 
      xreg = D_Sep2017,
      method = "ML")

arima(DLPax_Metro, order = c(1, 0, 1), 
      xreg = cbind(D_Sep2017, D_Oct2017),
      method = "ML")

#
