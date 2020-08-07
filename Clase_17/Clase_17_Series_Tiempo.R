# Series de Tiempo, Noviembre de 2020
# Clase 17. Cointegracion (1)
#****************************************************************************************
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("vars", dependencies = TRUE)
# 
library(ggplot2)
library(dplyr)
library(MASS)
library(strucchange)
library(zoo)
library(sandwich)
library(urca)
library(lmtest)
library(vars)

# 
#****************************************************************************************
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_17")
getwd()

#****************************************************************************************

load("Datos_Ad.RData")

#****************************************************************************************
# Conversion a series de tiempo:
#

Datos <- ts(Datos_Ad[7: 11], 
            start = c(2000, 1), 
            end = c(2019, 7), 
            freq = 12)

LDatos <- log(Datos)

DLDatos <- diff(log(Datos, base = exp(1)), 
                lag = 1, 
                differences = 1)

#****************************************************************************************
# Graficas:
#

plot(LDatos, 
     plot.type = "m", nc = 2,
     col = c("darkgreen", "darkblue", "darkred", "orange", "purple"), 
     main = "Series en Logaritmos", 
     xlab = "Tiempo")

#

plot(DLDatos, 
     plot.type = "m", nc = 2,
     col = c("darkgreen", "darkblue", "darkred", "orange", "purple"), 
     main = "Series en Diferencias Logaritmicas", 
     xlab = "Tiempo")

#

plot(cbind(LDatos, DLDatos), 
     plot.type = "m", nc = 2,
     col = c("darkgreen", "darkblue", "darkred", "orange", "purple"), 
     main = "Comparacion de Series en Diferencias", 
     xlab = "Tiempo")

#****************************************************************************************
# Pruebas de Raices Unitarias (ADF)
#****************************************************************************************
# ADF: Augmented Dickey - Fuller Test
# ur.df = function (y, type = c("none", "drift", "trend"), lags = 1, selectlags = c("Fixed", "AIC", "BIC")) 
# p = int{4*(T/100)^(1/4)}
#   = int{4*(234/100)^(1/4)}
#   = int{4.9475}
#   = 4

# INPC
# NIVELES
summary(ur.df(LDatos[, 1], type = "trend", lags = 4))

summary(ur.df(LDatos[, 1], type = "drift", lags = 4))

summary(ur.df(LDatos[, 1], type = "none", lags = 4))

# DIFERENCIAS
summary(ur.df(DLDatos[, 1], type = "trend", lags = 4))

summary(ur.df(DLDatos[, 1], type = "drift", lags = 4))

summary(ur.df(DLDatos[, 1], type = "none", lags = 4))

# Tipo de Cambio
# NIVELES
summary(ur.df(LDatos[, 2], type = "trend", lags = 4))

summary(ur.df(LDatos[, 2], type = "drift", lags = 4))

summary(ur.df(LDatos[, 2], type = "none", lags = 4))

# DIFERENCIAS
summary(ur.df(DLDatos[, 2], type = "trend", lags = 4))

summary(ur.df(DLDatos[, 2], type = "drift", lags = 4))

summary(ur.df(DLDatos[, 2], type = "none", lags = 4))

# CETES28
# NIVELES
summary(ur.df(LDatos[, 3], type = "trend", lags = 4))

summary(ur.df(LDatos[, 3], type = "drift", lags = 4))

summary(ur.df(LDatos[, 3], type = "none", lags = 4))

# DIFERENCIAS
summary(ur.df(DLDatos[, 3], type = "trend", lags = 4))

summary(ur.df(DLDatos[, 3], type = "drift", lags = 4))

summary(ur.df(DLDatos[, 3], type = "none", lags = 4))

# IGAE
# NIVELES
summary(ur.df(LDatos[, 4], type = "trend", lags = 4))

summary(ur.df(LDatos[, 4], type = "drift", lags = 4))

summary(ur.df(LDatos[, 4], type = "none", lags = 4))

# DIFERENCIAS
summary(ur.df(DLDatos[, 4], type = "trend", lags = 4))

summary(ur.df(DLDatos[, 4], type = "drift", lags = 4))

summary(ur.df(DLDatos[, 4], type = "none", lags = 4))

# IPI
# NIVELES
summary(ur.df(LDatos[, 5], type = "trend", lags = 4))

summary(ur.df(LDatos[, 5], type = "drift", lags = 4))

summary(ur.df(LDatos[, 5], type = "none", lags = 4))

# DIFERENCIAS
summary(ur.df(DLDatos[, 5], type = "trend", lags = 4))

summary(ur.df(DLDatos[, 5], type = "drift", lags = 4))

summary(ur.df(DLDatos[, 5], type = "none", lags = 4))

#****************************************************************************************
# VAR(p):
#****************************************************************************************
# ARGUMENTOS: 
# function (y, p = 1, 
# type = c("const", "trend", "both", "none"), 
# season = NULL, exogen = NULL, lag.max = NULL, 
# ic = c("AIC", "HQ", "SC", "FPE"))

# VAR(p) Seleccion:

VARselect(LDatos, lag.max = 10, type = "both")

VARselect(LDatos, lag.max = 10, type = "trend")

VARselect(LDatos, lag.max = 10, type = "const")

VARselect(LDatos, lag.max = 10, type = "none")

# VAR Estimacion:

VAR_1 <- VAR(LDatos, p = 3, type = "both")

summary(VAR_1)

summary(VAR_1, equation = "INPC_Ad")

VAR_1

plot(VAR_1, names = "INPC_Ad")

plot(VAR_1, names = "TC_Ad")

plot(VAR_1, names = "CETE28_Ad")

plot(VAR_1, names = "IGAE_Ad")

plot(VAR_1, names = "IPI_Ad")

#****************************************************************************************
# Cointegration Test:
#****************************************************************************************
# ca.jo = function (x, type = c("eigen", "trace"), ecdet = c("none", "const", 
# "trend"), K = 2, spec = c("longrun", "transitory"), season = NULL, 
# dumvar = NULL) 

summary(ca.jo(LDatos, type = "trace", ecdet = "trend", K = 3, spec = "longrun"))

summary(ca.jo(LDatos, type = "trace", ecdet = "const", K = 3, spec = "longrun"))

summary(ca.jo(LDatos, type = "trace", ecdet = "none", K = 3, spec = "longrun"))

#

CA_1 <- ca.jo(LDatos, type = "trace", ecdet = "const", K = 3, spec = "longrun")

summary(CA_1)

#
