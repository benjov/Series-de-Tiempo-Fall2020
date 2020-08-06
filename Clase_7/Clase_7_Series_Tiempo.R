# Series de Tiempo, Septiembre de 2020
# Clase 7. Aplicacion AR(p)
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_7")
getwd()

#****************************************************************************************

Datos <- read_excel("Base_Transporte.xlsx", sheet = "Datos", col_names = TRUE)

#****************************************************************************************
# Los datos importados son:
#
# Pax_Nal: Pasajeros (Salidas) en vuelos nacionales
# Pax_Int: Pasajeros (Salidas) en vuelos internacionales 
# Vue_Nal: Vuelos u operaciones (Salidas) en vuelos nacionales
# Vue_Int: Vuelos u operaciones (Salidas) en vuelos internacionales
# Pax_Metro: Pasajeros transportados (Millones) en el SCM
#
# Fuente: INEGI, https://www.inegi.org.mx/app/indicadores/?tm=0&t=1090
#****************************************************************************************
# Funciones que extrae y grafica las raices del polinomio caracteristico

source("arroots.R")
source("plot.armaroots.R")

#****************************************************************************************
# Conversion a series de tiempo:
# 

Pax_Int <- ts(Datos$Pax_Int, 
              start = c(2000, 1), 
              freq = 12)

# Logaritmos:
LPax_Int <- ts(log(Datos$Pax_Int), 
               start = c(2000, 1), 
               freq = 12)

# Diferencias mensuales:
DLPax_Int <- ts(log(Datos$Pax_Int) - lag(log(Datos$Pax_Int), k = 1),
                start = c(2000, 1), 
                freq = 12)

#****************************************************************************************
# Graficas:
par(mfrow=c(3,1))

plot(Pax_Int, xlab = "Tiempo", ylab = "Pasajeros",
     main = "Pasajeros en vuelos internacionales de salida",
     col = "darkgreen")

plot(LPax_Int, xlab = "Tiempo", ylab = "LN Pasajeros",
     main = "LN Pasajeros en vuelos internacionales de salida",
     col = "darkblue")

plot(DLPax_Int, xlab = "Tiempo", ylab = "DLN Pasajeros",
     main = "Diff LN Pasajeros en vuelos internacionales de salia", 
     col = "darkred")

par(mfrow=c(1,1))

#****************************************************************************************
# Estimacion del proceo AR(p):

# AR(p) en NIVELES:

AR_LPax_Int <- arima(LPax_Int, order = c(4, 0, 0), method = "ML")

AR_LPax_Int

plot(AR_LPax_Int$residuals)

plot.armaroots(arroots(AR_LPax_Int), 
               main="Inverse AR roots of \nAR(p): LN PAx Int")

# AR(p) en DIFERENCIAS:

AR_DLPax_Int <- arima(DLPax_Int, order = c(4, 0, 0), method = "ML")

AR_DLPax_Int

plot(AR_DLPax_Int$residuals, xlab = "Tiempo", ylab = "DLN Pasajeros",
     main = "Residuales de un AR(4) para pasajeros en vuelos internacionales de salida",
     col = "darkblue")

plot.armaroots(arroots(AR_DLPax_Int), 
               main="Inverse AR roots of \nAR(p): Diff LN PAx Int")

#****************************************************************************************
# Función de Autocorrelación Parcial

pacf(LPax_Int[1:234], lag.max = 14,
     main = "Función de Autocorrelación Parcial de pasajeros en vuelos internacionales de salida",
     xlab = 'Rezagos en k meses')

pacf(DLPax_Int[2:234], lag.max = 14, 
     main = "Función de Autocorrelación Parcial de pasajeros en vuelos internacionales de salida",
     xlab = 'Rezagos en k meses')

#
