# Series de Tiempo, Octubre de 2020
# Clase 14. VAR(p)
#****************************************************************************************
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("stats")
#install.packages("vars", dependencies = TRUE)
# 
library(ggplot2)
library(dplyr)
library(stats)
library(MASS)
library(strucchange)
library(zoo)
library(sandwich)
library(urca)
library(lmtest)
library(vars)

# 
#****************************************************************************************
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_14")
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

DLINPC <- ts(log(Datos_Ad$INPC_Ad) - lag(log(Datos_Ad$INPC_Ad), k = 1), 
             start = c(2000, 1), 
             freq = 12)

#

DLTC <- ts(log(Datos_Ad$TC_Ad) - lag(log(Datos_Ad$TC_Ad), k = 1), 
           start = c(2000, 1), 
           freq = 12)

#

DLCETE28 <- ts(log(Datos_Ad$CETE28_Ad) - lag(log(Datos_Ad$CETE28_Ad), k = 1), 
               start = c(2000, 1), 
               freq = 12)

#

DLIGAE <- ts(log(Datos_Ad$IGAE_Ad) - lag(log(Datos_Ad$IGAE_Ad), k = 1), 
             start = c(2000, 1), 
             freq = 12)

#

DLIPI <- ts(log(Datos_Ad$IPI_Ad) - lag(log(Datos_Ad$IPI_Ad), k = 1), 
            start = c(2000, 1), 
            freq = 12)

# Combinacion en un solo set de datos

Datos <- data.frame(cbind(DLINPC, DLTC, DLCETE28, DLIGAE, DLIPI))

Datos <- ts(Datos[2:235, ], 
            start = c(2000, 2), freq = 12)

#****************************************************************************************
# Graficas:

plot(Datos, plot.type = "s", 
     col = c("darkgreen", "darkblue", "darkred", "black", "purple"), 
     main = "Series en Diferencias logaritmicas", 
     xlab = "Tiempo", ylab = "Variacion")

legend("bottomright", c("INPC", "TC", "CETES28", "IGAE", "IPI"),
       cex = 0.6, lty = 1:1, 
       col = c("darkgreen", "darkblue", "darkred", "black", "purple"))

#
plot(Datos, plot.type = "m", 
     col = "darkgreen", 
     main = "Series en Diferencias logaritmicas", xlab = "Tiempo")

#****************************************************************************************
# 1. VAR(p):
# ARGUMENTOS: 
# function (y, p = 1, 
# type = c("const", "trend", "both", "none"), 
# season = NULL, exogen = NULL, lag.max = NULL, 
# ic = c("AIC", "HQ", "SC", "FPE"))

VAR(Datos, p = 1)

VAR(Datos[ , 1:3], p = 1)

VAR(Datos[ , c(1, 2, 4, 5)], p = 1, exogen = Datos[ , 3])

VAR(Datos, p = 2)

VAR(Datos, p = 3)

VAR(Datos, p = 4)

# 

VAR01 <- VAR(Datos, p = 4, type = "none")

names(VAR01)

summary(VAR01)

summary(VAR01, equation = "DLIGAE")

roots(VAR01)

# Seleccion de VAR

VARselect(Datos, lag.max = 12, type = "both")

VARselect(Datos, lag.max = 12, type = "const")

VARselect(Datos, lag.max = 12, type = "none")

# Var OPTIMO:

VAR02 <- VAR(Datos, p = 2)

summary(VAR02)

summary(VAR02, equation = "DLIGAE")

summary(VAR02, equation = "DLINPC")

summary(VAR02, equation = "DLCETE28")

#
ss