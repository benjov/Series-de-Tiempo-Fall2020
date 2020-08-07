# Series de Tiempo, Octubre de 2020
# Clase 13. Causalidad de Granger
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_13")
getwd()

#****************************************************************************************

load("Datos_Ad.RData")

#****************************************************************************************
# Los datos "cargados" son:
#
# INPC: Indice Nacional de Precios al Consumidor (2QJul2018 = 100)
# TC: Tipo de Cambio FIX 
# CETE28: Tasa de rendimiento promedio mensual de los Cetes 28, en por ciento anual
# IGAE: Indicador global de la actividad económica (2013 = 100)
# IPI: Industrial Production Index (2012 = 100)
#
# Asi como cada una de las series desestacionalizadas
#
#****************************************************************************************
# Conversion a series de tiempo:
#

names(Datos_Ad)

#

INPC <- ts(Datos_Ad$INPC_Ad, 
           start = c(2000, 1), 
           freq = 12)

DLINPC <- ts(log(Datos_Ad$INPC_Ad) - lag(log(Datos_Ad$INPC_Ad), k = 1), 
             start = c(2000, 1), 
             freq = 12)

#

TC <- ts(Datos_Ad$TC_Ad, 
         start = c(2000, 1), 
         freq = 12)

DLTC <- ts(log(Datos_Ad$TC_Ad) - lag(log(Datos_Ad$TC_Ad), k = 1), 
           start = c(2000, 1), 
           freq = 12)

#

CETE28 <- ts(Datos_Ad$CETE28_Ad, 
             start = c(2000, 1), 
             freq = 12)

DLCETE28 <- ts(log(Datos_Ad$CETE28_Ad) - lag(log(Datos_Ad$CETE28_Ad), k = 1), 
               start = c(2000, 1), 
               freq = 12)

#****************************************************************************************
# Graficas:

par(mfrow=c(3, 1))

plot(DLINPC, xlab = "Tiempo", 
     main = "Diferencias Logarítmicas del INPC",
     col = "darkgreen")

plot(DLTC, xlab = "Tiempo", 
     main = "Diferencias Logarítmicas del Tipo de Cambio",
     col = "darkblue")

plot(DLCETE28, xlab = "Tiempo", 
     main = "Diferencias Logarítmicas de los Cetes a 28 dias",
     col = "darkred")

par(mfrow=c(1, 1))

#****************************************************************************************
# Prueba de Causalidad de Granger
#
#install.packages("zoo")
#install.packages("lmtest")
#
library(zoo)
library(lmtest)

# Combinacion en un solo set de datos
Datos <- data.frame(cbind(DLINPC, DLTC, DLCETE28))

Datos <- cbind(Datos_Ad["FECHA"], Datos)

names(Datos)

# Las pruebas de causalidad para 4, 8, 12 y 16 rezagos:

# 4
grangertest(DLINPC ~ DLTC, order = 4, data = Datos)

grangertest(DLTC ~ DLINPC, order = 4, data = Datos)

grangertest(DLINPC ~ DLCETE28, order = 4, data = Datos)

grangertest(DLCETE28 ~ DLINPC, order = 4, data = Datos)

grangertest(DLTC ~ DLCETE28, order = 4, data = Datos)

grangertest(DLCETE28 ~ DLTC, order = 4, data = Datos)

# 8
grangertest(DLINPC ~ DLTC, order = 8, data = Datos)

grangertest(DLTC ~ DLINPC, order = 8, data = Datos)

grangertest(DLINPC ~ DLCETE28, order = 8, data = Datos)

grangertest(DLCETE28 ~ DLINPC, order = 8, data = Datos)

grangertest(DLTC ~ DLCETE28, order = 8, data = Datos)

grangertest(DLCETE28 ~ DLTC, order = 8, data = Datos)

# 12
grangertest(DLINPC ~ DLTC, order = 12, data = Datos)

grangertest(DLTC ~ DLINPC, order = 12, data = Datos)

grangertest(DLINPC ~ DLCETE28, order = 12, data = Datos)

grangertest(DLCETE28 ~ DLINPC, order = 12, data = Datos)

grangertest(DLTC ~ DLCETE28, order = 12, data = Datos)

grangertest(DLCETE28 ~ DLTC, order = 12, data = Datos)

# 16
grangertest(DLINPC ~ DLTC, order = 16, data = Datos)

grangertest(DLTC ~ DLINPC, order = 16, data = Datos)

grangertest(DLINPC ~ DLCETE28, order = 16, data = Datos)

grangertest(DLCETE28 ~ DLINPC, order = 16, data = Datos)

grangertest(DLTC ~ DLCETE28, order = 16, data = Datos)

grangertest(DLCETE28 ~ DLTC, order = 16, data = Datos)

#
