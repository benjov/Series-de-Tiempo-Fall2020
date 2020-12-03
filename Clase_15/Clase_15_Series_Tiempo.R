# Series de Tiempo, Octubre de 2020
# Clase 15. VAR(p) - Analisis de Impulso-Respuesta
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_15")
getwd()

#****************************************************************************************

load("Datos_Ad.RData")

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
            start = c(2000, 2), 
            freq = 12)

#****************************************************************************************
# VAR(p) Selection:

VARselect(Datos, lag.max = 12, type = "const")

# VAR Estimacion:

VAR_p <- VAR(Datos, p = 2, type = "const")

summary(VAR_p)

summary(VAR_p, equation = "DLINPC")

summary(VAR_p, equation = "DLTC")

summary(VAR_p, equation = "DLCETE28")

summary(VAR_p, equation = "DLIGAE")

summary(VAR_p, equation = "DLIPI")

#****************************************************************************************
# Diagnostic tests

# Normalidad:
normality.test(VAR_p)

# Autocorrelacion Serial:
serial.test(VAR_p, lags.bg = 2, type = "BG")

serial.test(VAR_p, lags.bg = 4, type = "BG")

serial.test(VAR_p, lags.bg = 6, type = "BG")

# Homocedasticidad
arch.test(VAR_p, lags.multi = 2)

#****************************************************************************************
# Impulse Response:

? irf

#

IR_DLINPC <- irf(VAR_p, n.ahead = 12, boot = TRUE, 
                 ci = 0.95, response = "DLINPC")

IR_DLINPC

plot(IR_DLINPC)

#

IR_DLTC <- irf(VAR_p, n.ahead = 12, boot = TRUE, 
               ci = 0.95, response = "DLTC")

IR_DLTC

plot(IR_DLTC)

#

IR_DLCETE28 <- irf(VAR_p, n.ahead = 12, boot = TRUE,
                   ci = 0.95, response = "DLCETE28")

IR_DLCETE28

plot(IR_DLCETE28)

#

IR_DLIPI <- irf(VAR_p, n.ahead = 12, boot = TRUE, 
                ci = 0.95, response = "DLIPI")

IR_DLIPI

plot(IR_DLIPI)

#**********************************************************

IR_DLINPC_2 <- irf(VAR_p, n.ahead = 12, boot = TRUE, 
                   ci = 0.95, response = "DLINPC",
                   ortho = TRUE, cumulative = FALSE)

plot(IR_DLINPC_2)

IR_DLINPC_3 <- irf(VAR_p, n.ahead = 12, boot = TRUE, 
                   ci = 0.95, response = "DLINPC",
                   ortho = TRUE, cumulative = TRUE)

plot(IR_DLINPC_3)
