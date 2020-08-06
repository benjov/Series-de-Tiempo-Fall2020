# Series de Tiempo, Septiembre de 2020
# Clase 5. Aplicacion AR(1)
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_5")
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
# Conversion a series de tiempo:
# 
Dat_Aereo <- ts(Datos[c("Pax_Nal", "Pax_Int", "Vue_Nal", "Vue_Int")], 
                start = c(2000, 1), 
                end = c(2019, 6), 
                freq = 12)

Pax_Metro <- ts(Datos$Pax_Metro, start = c(2000, 1), 
                end = c(2019, 6), 
                freq = 12)

# Logaritmos:
Dat_LAereo <- ts(log(Datos[c("Pax_Nal", "Pax_Int", "Vue_Nal", "Vue_Int")]), 
                start = c(2000, 1), 
                end = c(2019, 6), 
                freq = 12)

Pax_LMetro <- ts(log(Datos$Pax_Metro), start = c(2000, 1), 
                end = c(2019, 6), 
                freq = 12)

# Diferencias mensuales:

Pax_DLMetro <- ts( log(Datos$Pax_Metro) - lag(log(Datos$Pax_Metro), k = 1),
                 start = c(2000, 1), freq = 12)

#****************************************************************************************
# Graficas:

plot(Pax_Metro, xlab = "Tiempo", 
     main = "Pasajeros transportados (Millones) en el SCM",
     col = "darkgreen")

plot(Pax_LMetro, xlab = "Tiempo", 
     main = "LN Pasajeros transportados (Millones) en el SCM",
     col = "darkblue")

plot(Pax_DLMetro, xlab = "Tiempo", 
     main = "Diff LN Pasajeros transportados (Millones) en el SCM", 
     col = "darkred")

#
par(mfrow = c(3,1))

plot(Pax_Metro, xlab = "Tiempo", 
     main = "Pasajeros transportados (Millones) en el SCM",
     col = "darkgreen")

plot(Pax_LMetro, xlab = "Tiempo", 
     main = "LN Pasajeros transportados (Millones) en el SCM",
     col = "darkblue")

plot(Pax_DLMetro, xlab = "Tiempo", 
     main = "Diff LN Pasajeros transportados (Millones) en el SCM", 
     col = "darkred")

par(mfrow=c(1,1))

#****************************************************************************************
# Estimacion del proceo AR(1):
# Utilizamos la función arima:

# Serie en NIVELES:
arima(Pax_LMetro, order = c(1, 0, 0), method = "ML")

AR_Pax_LMetro <- arima(Pax_LMetro, order = c(1, 0, 0), method = "ML")

AR_Pax_LMetro

names(AR_Pax_LMetro)

mean(AR_Pax_LMetro$residuals)

plot(AR_Pax_LMetro$residuals, 
     col = "darkred",
     xlab = "Tiempo", 
     ylab = "",
     main = "Residuales de un AR(1) para el LN de los pasajeros del metro de la CDMX")

# Serie en DIFERENCIAS:
arima(Pax_DLMetro, order = c(1, 0, 0), method = "ML")

arima(Pax_LMetro, order = c(1, 1, 0), method = "ML")

AR_Pax_DLMetro <- arima(Pax_DLMetro, order = c(1, 0, 0), method = "ML")

AR_Pax_DLMetro

names(AR_Pax_DLMetro)

mean(AR_Pax_DLMetro$residuals)

plot(AR_Pax_DLMetro$residuals, 
     col = "darkred",
     xlab = "Tiempo", 
     ylab = "",
     main = "Residuales de un AR(1) para la diferencia del LN de los pasajeros del metro de la CDMX")

#
