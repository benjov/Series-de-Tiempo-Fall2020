# Series de Tiempo, Octubre de 2020
# Clase 12.  Modelling the output gap - Hodrick-Prescott filter 
# Estos modelos fueron desarrollados por Hodrick & Prescott (1997)
# Suggested values -Constants:
# Lambda = 100 for yearly data
# Lambda = 1600 for quarterly data
# Lambda = 14400 for monthly data
#****************************************************************************************
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages(stats)
#install.packages("mFilter")
# 
library(ggplot2)
library(dplyr)
library(readxl)
library(stats)
library(mFilter)
# 
#****************************************************************************************
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_12")
getwd()

#****************************************************************************************
load("Datos_Ad.RData")

#****************************************************************************************
# Los datos "cargados" son los originales y los ajustados por estacionalidad):
#
# INPC: Indice Nacional de Precios al Consumidor (2QJul2018 = 100)
# TC: Tipo de Cambio FIX 
# CETE28: Tasa de rendimiento promedio mensual de los Cetes 28, en por ciento anual
# IGAE: Indicador global de la actividad económica (2013 = 100)
# IPI: Industrial Production Index (2012 = 100)
#
#****************************************************************************************
# Conversion a series de tiempo:
#

INPC <- ts(Datos_Ad$INPC_Ad, 
           start = c(2000, 1), 
           freq = 12)

TC <- ts(Datos_Ad$TC_Ad, 
         start = c(2000, 1), 
         freq = 12)

#****************************************************************************************
# Filtro Hodrick-Prescott:

# INPC:

? hpfilter

INPC_hpf <- hpfilter(INPC, freq = 14400)

names(INPC_hpf)

INPC_hpf$cycle

INPC_hpf$trend

INPC_hpf$lambda

INPC_hpf$x

plot(INPC_hpf)

INPC_Cycle <- INPC_hpf$cycle

INPC_Trend <- INPC_hpf$trend

# TC:

TC_hpf <- hpfilter(TC, freq = 14400)

plot(TC_hpf)

TC_Cycle <- TC_hpf$cycle

TC_Trend <- TC_hpf$trend

# DATAFRAME:

Datos_HPF <- cbind(Datos_Ad$FECHA, 
                  data.frame(cbind(INPC, INPC_Trend, INPC_Cycle, 
                                   TC, TC_Trend, TC_Cycle)))

#
