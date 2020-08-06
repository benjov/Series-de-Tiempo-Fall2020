# Series de Tiempo, Septiembre de 2020
# Clase 9. Aplicacion ARIMA(p, d, q) y ARMA(p, q)
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_9")
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

#****************************************************************************************
# Estimacion de ARIMA(1, 1, 1)

ARMA_DLPax_Nal <- arima(DLPax_Nal, order = c(1, 0, 1),
                        xreg = cbind(D_Ene, D_Feb, D_Jul, D_Dic),
                        method = "ML")

ARMA_DLPax_Nal

#

plot(ARMA_DLPax_Nal$residuals,
     main = "Residuales de un ARIMA LN Pasajeros en vuelos nacionales de salida",
     col = "darkblue",
     ylab = "Residuals ARMA(1, 1)")

#

par(mfrow=c(1,2))

plot.armaroots(arroots(ARMA_DLPax_Nal), 
               main="Inverse AR roots of \nAR(p): LN PAx Nal")

plot.armaroots(maroots(ARMA_DLPax_Nal), 
               main="Inverse MA roots of \nMA(q): LN PAx Nal")

par(mfrow=c(1,1))

#****************************************************************************************
# Gráficas de las funciones de Autocorrelacion:

par(mfrow=c(1,2))

acf(DLPax_Nal[2:234], lag.max = 12,
    xlab = "Rezagos",
    main = "Diff LN Pasajeros Nacionales")

pacf(DLPax_Nal[2:234], lag.max = 12, 
     xlab = 'Rezagos',
     main = "Diff LN Pasajeros Nacionales")

par(mfrow=c(1,1))

#****************************************************************************************
# Funciones que determinan el rezago optimo (que minimiza el criterio de Akaike)
# Esta es una prueba mas formar respecto de la meta inspeccion grafica

source("Lag_Opt_ARIMA_Exog.R")

# La funcion necesita de parametros definidos como:
  # Lag_Opt_ARIMA_Ex(p_max, d_max, q_max, X_t, Ex, Z_t){
  # p_max: Rezagos maximos a evaluar del componente AR
  # q_max: Rezagos maximos a evaluar del componente MA
  # d_max: Numero de diferencias evualadas
  # X_t: Serie de Tiempo modelada
  # Z_t: Vector o Matriz de variables exogenas
  # Ex: es indicador de que incluye variables exogenas (0 indica que no se incluyen, y 1 que si)

#****************************************************************************************
# Estimacion del proceso ARIMA(p, 1, q):

# CON variables exogenas:
Lag_Opt_ARIMA_Exog(p_max = 6, q_max = 6, 
                   X_t = DLPax_Nal, 
                   Ex = 1,
                   Z_t = cbind(D_Ene, D_Feb, D_Jul, D_Dic))

# SIN variables exogenas:
Lag_Opt_ARIMA_Exog(p_max = 6, q_max = 6, 
                   X_t = DLPax_Nal, 
                   Ex = 0,
                   Z_t = cbind(D_Ene, D_Jul, D_Dic))

# Estimacion:

ARMA_Ex_DLPax_Nal <- arima(DLPax_Nal, order = c(4, 0, 6),
                           xreg = cbind(D_Ene, D_Feb, D_Jul, D_Dic),
                           method = "ML")

ARMA_Ex_DLPax_Nal

plot(ARMA_Ex_DLPax_Nal$residuals, 
     ylab = "",
     main = "Residuales ARMA Diff LN Pasajeros Nacionales",
     col = "darkblue")

#****************************************************************************************
# Nueva Estimacion del proceso ARIMA(p, 1, q):

# CON variables exogenas rezagos optimos:
Lag_Opt_ARIMA_Exog(p_max = 6, q_max = 6, 
                   X_t = DLPax_Nal, 
                   Ex = 1,
                   Z_t = cbind(D_Ene, D_Feb, D_Jul, D_Dic, D_May2009, D_Jun2009))

# Estimacion:

ARMA_Ex_DLPax_Nal_2 <- arima(DLPax_Nal, order = c(4, 0, 6),
                             xreg = cbind(D_Ene, D_Feb, D_Jul, D_Dic, D_May2009, D_Jun2009),
                             method = "ML")

ARMA_Ex_DLPax_Nal_2

plot(ARMA_Ex_DLPax_Nal_2$residuals, 
     ylab = "",
     main = "Residuales ARMA Diff LN Pasajeros Nacionales",
     col = "darkblue")

#

par(mfrow=c(1,2))

plot.armaroots(arroots(ARMA_Ex_DLPax_Nal_2), 
               main="Inverse AR roots of \nAR(p): LN PAx Nal")

plot.armaroots(maroots(ARMA_Ex_DLPax_Nal_2), 
               main="Inverse MA roots of \nMA(q): LN PAx Nal")

par(mfrow=c(1,1))

#
