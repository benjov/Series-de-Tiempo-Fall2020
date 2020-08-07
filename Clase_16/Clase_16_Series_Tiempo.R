# Series de Tiempo, Noviembre de 2020
# Clase 16. Procesos No Estacionarios (Pruebas de Raices Unitarias)
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_16")
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

DaLDatos <- diff(log(Datos, base = exp(1)), 
                 lag = 12, 
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

plot(DaLDatos, 
     plot.type = "m", nc = 2,
     col = c("darkgreen", "darkblue", "darkred", "orange", "purple"), 
     main = "Series en Diferencias Anuales Logaritmicas", 
     xlab = "Tiempo")

#

plot(cbind(DLDatos, DaLDatos), 
     plot.type = "m", nc = 2,
     col = c("darkgreen", "darkblue", "darkred", "orange", "purple"), 
     main = "Comparacion de Series en Diferencias", 
     xlab = "Tiempo")
#

plot(cbind(LDatos, DLDatos), 
     plot.type = "m", nc = 2,
     col = c("darkgreen", "darkblue", "darkred", "orange", "purple"), 
     main = "Comparacion de Series en Diferencias", 
     xlab = "Tiempo")

#****************************************************************************************
# Pruebas de Raices Unitarias
#****************************************************************************************
# 1. ADF: Augmented Dickey - Fuller Test
# ur.df = function (y, type = c("none", "drift", "trend"), lags = 1, selectlags = c("Fixed", "AIC", "BIC")) 

#############################
# Dickey-Fuller:
#############################

# NIVELES: Tipo de cambio
summary(ur.df(LDatos[, 2], type = "trend", lags = 0))

summary(ur.df(LDatos[, 2], type = "drift", lags = 0))

summary(ur.df(LDatos[, 2], type = "none", lags = 0))

# DIFERENCIAS: Tipo de cambio
summary(ur.df(DLDatos[, 2], type = "trend", lags = 0))

summary(ur.df(DLDatos[, 2], type = "drift", lags = 0))

summary(ur.df(DLDatos[, 2], type = "none", lags = 0))

#############################
# Augmented Dickey - Fuller
#############################
# p = int{4*(T/100)^(1/4)}
#   = int{4*(234/100)^(1/4)}
#   = int{4.9475}
#   = 4

# NIVELES: Tipo de cambio
summary(ur.df(LDatos[, 2], type = "trend", lags = 4))

summary(ur.df(LDatos[, 2], type = "drift", lags = 4))

summary(ur.df(LDatos[, 2], type = "none", lags = 4))

# DIFERENCIAS: Tipo de cambio
summary(ur.df(DLDatos[, 2], type = "trend", lags = 4))

summary(ur.df(DLDatos[, 2], type = "drift", lags = 4))

summary(ur.df(DLDatos[, 2], type = "none", lags = 4))

#****************************************************************************************
# 2. PP: Phillips - Perron Test
# ur.pp = function (x, type = c("Z-alpha", "Z-tau"), model = c("constant", "trend"), lags = c("short", "long"), use.lag = NULL) 

# NIVELES: Tipo de cambio
summary(ur.pp(LDatos[, 2], type = "Z-tau", model = "trend", use.lag = 4))

summary(ur.pp(LDatos[, 2], type = "Z-tau", model = "constant", use.lag = 4))

# DIFERENCIAS: Tipo de cambio
summary(ur.pp(DLDatos[, 2], type = "Z-tau", model = "trend", use.lag = 4))

summary(ur.pp(DLDatos[, 2], type = "Z-tau", model = "constant", use.lag = 4))

#****************************************************************************************
# 3. KPSS: 
# ur.kpss = function (y, type = c("mu", "tau"), lags = c("short", "long", "nil"), use.lag = NULL)

# NIVELES: Tipo de cambio

summary(ur.kpss(LDatos[, 2], type = "tau"))

summary(ur.kpss(LDatos[, 2], type = "mu"))

# NIVELES: Tipo de cambio

summary(ur.kpss(DLDatos[, 2], type = "tau", use.lag = 4))

summary(ur.kpss(DLDatos[, 2], type = "mu", use.lag = 4))

#
