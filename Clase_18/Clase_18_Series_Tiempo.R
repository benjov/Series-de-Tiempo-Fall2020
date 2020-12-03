# Series de Tiempo, Noviembre de 2020
# Clase 18. Cointegracion (2)
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_18")
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

plot(cbind(LDatos, DLDatos), 
     plot.type = "m", nc = 2,
     col = c("darkgreen", "darkblue", "darkred", "orange", "purple"), 
     main = "Comparacion de Series en Diferencias", 
     xlab = "Tiempo")

#****************************************************************************************
# Condicional en que todas las series son I(1)
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

plot(VAR_1)

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

# Prueba basada en la TRAZA

summary(ca.jo(LDatos, type = "trace", ecdet = "trend", K = 3, spec = "longrun"))

summary(ca.jo(LDatos, type = "trace", ecdet = "const", K = 3, spec = "longrun"))

summary(ca.jo(LDatos, type = "trace", ecdet = "none", K = 3, spec = "longrun"))

# Prueba basada en el Max Eigen Value:

summary(ca.jo(LDatos, type = "eigen", ecdet = "trend", K = 3, spec = "longrun"))

summary(ca.jo(LDatos, type = "eigen", ecdet = "const", K = 3, spec = "longrun"))

summary(ca.jo(LDatos, type = "eigen", ecdet = "none", K = 3, spec = "longrun"))

#

CA_1 <- ca.jo(LDatos, type = "trace", ecdet = "const", K = 3, spec = "longrun")

summary(CA_1)

# Residuales:

TT <- ts(c(1:235), 
         start = c(2000, 1), 
         end = c(2019, 7), 
         freq = 12)

#U <- LDatos[ , 1] + 0.151162436*LDatos[ , 2] -
#     0.042650912*LDatos[ , 3] + 0.163804862*LDatos[ , 4] +
#     0.229295743*LDatos[ , 5] - 0.004350646*TT

U <- LDatos[ , 1] + 0.32902416*LDatos[ , 2] -
  0.09226732*LDatos[ , 3] - 3.14150738*LDatos[ , 4] +
  1.98701625*LDatos[ , 5] - 0.19972818


plot(U, 
     main = "Residuales de la Ecuación de Cointegración",
     type = "l", 
     col = "darkred")

# Raices Unitarias
# NIVELES
summary(ur.df(U, type = "trend", lags = 4))

summary(ur.df(U, type = "drift", lags = 4))

summary(ur.df(U, type = "none", lags = 4))

# DIFERENCIAS
summary(ur.df(DLDatos[, 5], type = "trend", lags = 4))

summary(ur.df(DLDatos[, 5], type = "drift", lags = 4))

summary(ur.df(DLDatos[, 5], type = "none", lags = 4))

#
