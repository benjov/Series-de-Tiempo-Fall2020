# Series de Tiempo, Octubre de 2020
# Clase 11. Aplicacion Modelos de Desestacionalización
# Estos modelos fueron desarrollados por el https://www.census.gov/srd/www/x13as/
# Existe un paquete descargable y que no requiere de instalacion disponible en:
# https://cran.r-project.org/web/packages/x13binary/index.html
#****************************************************************************************
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages(stats)
#install.packages("seasonal")
#install.packages("seasonalview")
#install.packages("shiny")
# 
library(ggplot2)
library(dplyr)
library(readxl)
library(stats)
library(seasonal)
library(seasonalview)
library(shiny)
# 
#****************************************************************************************
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_11")
getwd()

#****************************************************************************************
Datos <- read_excel("Base_VAR.xlsx", sheet = "Datos", col_names = TRUE)

#****************************************************************************************
# Los datos importados son:
#
# INPC: Indice Nacional de Precios al Consumidor (2QJul2018 = 100)
# TC: Tipo de Cambio FIX 
# CETE28: Tasa de rendimiento promedio mensual de los Cetes 28, en por ciento anual
# IGAE: Indicador global de la actividad económica (2013 = 100)
# IPI: Industrial Production Index (2012 = 100)
#
#****************************************************************************************
# Primer ejemplo:
# 

INPC <- ts(Datos$INPC, 
           start = c(2000, 1), 
           freq = 12)

# Desestacionalizacion (forma complicada):

? seas

Seas_INPC <- seas(INPC)

names(Seas_INPC)

summary(Seas_INPC)

final(Seas_INPC)

original(Seas_INPC)

cbind(original(Seas_INPC), final(Seas_INPC))

plot(Seas_INPC)

# Foma Facil:

view(Seas_INPC)

INPC_Ad <- final(Seas_INPC)

#****************************************************************************************
# Conversion a series de tiempo y desestacionalizacion:
# 

TC <- ts(Datos$TC, 
         start = c(2000, 1),
         freq = 12)

Seas_TC <- seas(TC)

plot(Seas_TC)

TC_Ad <- final(Seas_TC)

#

CETE28 <- ts(Datos$CETE28, 
             start = c(2000, 1), 
             freq = 12)

Seas_CETE28 <- seas(CETE28)

plot(Seas_CETE28)

CETE28_Ad <- final(Seas_CETE28)

#

IGAE <- ts(Datos$IGAE, 
           start = c(2000, 1), 
           freq = 12)

Seas_IGAE <- seas(IGAE)

plot(Seas_IGAE)

IGAE_Ad <- final(Seas_IGAE)

#

IPI <- ts(Datos$IPI, 
          start = c(2000, 1), 
          freq = 12)

Seas_IPI <- seas(IPI)

plot(Seas_IPI)

IPI_Ad <- final(Seas_IPI)

#

#****************************************************************************************
# Agregando nuevas series desestacionalizadas:
# 

Datos_Ad <- data.frame(cbind(INPC_Ad, TC_Ad, CETE28_Ad, IGAE_Ad, IPI_Ad))

Datos_Ad <- cbind(Datos, Datos_Ad)

save(Datos_Ad, file = "Datos_Ad.RData")

####

load("Datos_Ad.RData")

#
