# Series de Tiempo, Septiembre de 2020
# Clase 6. Aplicacion AR(2)
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_6")
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

Pax_Nal <- ts(Datos$Pax_Nal, 
              start = c(2000, 1),
              end = c(2019, 6), 
              freq = 12)

# Logaritmos:
LPax_Nal <- ts(log(Datos$Pax_Nal), 
               start = c(2000, 1), 
               end = c(2019, 6), 
               freq = 12)

# Diferencias mensuales:
DLPax_Nal <- ts(log(Datos$Pax_Nal) - lag(log(Datos$Pax_Nal), k = 1),
                start = c(2000, 1), freq = 12)

#****************************************************************************************
# Graficas:
par(mfrow=c(3,1))

plot(Pax_Nal, xlab = "Tiempo", ylab = "Pasajeros",
     main = "Pasajeros en vuelos nacionales de salida",
     col = "darkgreen")

plot(LPax_Nal, xlab = "Tiempo", ylab = "LN Pasajeros",
     main = "LN Pasajeros en vuelos nacionales de salida",
     col = "darkblue")

plot(DLPax_Nal, xlab = "Tiempo", ylab = "DLN Pasajeros",
     main = "Diff LN Pasajeros en vuelos nacionales de salida", 
     col = "darkred")

par(mfrow=c(1,1))

#****************************************************************************************
# Estimacion del proceo AR(2):

# AR(2) en NIVELES:

AR_LPax_Nal <- arima(LPax_Nal, order = c(2, 0, 0), method = "ML")

AR_LPax_Nal

plot(AR_LPax_Nal$residuals, 
     xlab = "Tiempo", ylab = "",
     main = "Residuales del AR (2) para LN de pasajeros en vuelos nacionales de salida",
     col = "darkgreen")

# AR(2) en DIFERENCIAS:

AR_DLPax_Nal <- arima(DLPax_Nal, order = c(2, 0, 0), method = "ML")

AR_DLPax_Nal

plot(AR_DLPax_Nal$residuals, 
     xlab = "Tiempo", ylab = "",
     main = "Residuales del AR (2) para DLN de pasajeros en vuelos nacionales de salida",
     col = "darkred")

#****************************************************************************************
# Función que extrae las raices del polinomio caracteristico y las grafica
source("arroots.R")

source("plot.armaroots.R")

#****************************************************************************************

# Graficas raíces:
par(mfrow=c(1,2))

plot.armaroots(arroots(AR_LPax_Nal), 
               main="Inverse AR roots of \nAR(2): LN Pax Nal")

#
plot.armaroots(arroots(AR_DLPax_Nal), 
               main="Inverse AR roots of \nAR(2): Diff LN Pax Nal")

par(mfrow=c(1,1))

#
