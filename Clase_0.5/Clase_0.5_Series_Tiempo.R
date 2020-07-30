# Series de Tiempo, Agosto de 2020
# Clase 0.5. Introduccion

#****************************************************************************************
getwd()
# Cambiar la siguiente ruta dependiendo de la ubicación en sus equipos
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I")

getwd()

#****************************************************************************************
# Instalamos y Usamos la libreria de lectura de archivos
# R es un ambiente colaborativo, por ello los Packages son muchas veces desarrollados 
# por otros usuarios

#install.packages("readxl")

library(readxl)

# La siguiente linea permite importar datos desde un archivo XLSX, especificando la hoja 

Base_1 <- read_excel("Base_1_TimeSeries.xlsx", sheet = "Hoja 1", col_names = TRUE)

# Los datos importados son:
# IGAE_2013:      Indicador global de la actividad economica, base 2013
# IGAE_PRIM_2013: Indicador global de la actividad economica, base 2013, Actividades primarias
# ICC_2003:       Indice de confianza del consumidor, base 2003
# ICC_LAG_2003:   Indice de confianza del consumidor, comparada con la situacion economica 
#                 que los miembros del hogar tenian hace 12 meses 
# IPC_BMV:        Indice de precios y cotizaciones de la bolsa mexicana de valores 
#                 Ultimo del mes (Indice base octubre 1978 = 0.78)
# TDC:            Tipo de cambio del peso respecto al dolar, Interbancario (Pesos)

# Visualizamos los datos de la tabla

View(Base_1)

head(Base_1)

str(Base_1)

names(Base_1)

#****************************************************************************************
# Declaramos las base de datos que se acaba de importar como varias series de tiempo:

IGAE_2013 <- ts(Base_1$IGAE_2013, start = 2002, freq = 12)
IGAE_PRIM_2013 <- ts(Base_1$IGAE_PRIM_2013, start = 2002, freq = 12) 
ICC_2003 <- ts(Base_1$ICC_2003, start = 2002, freq = 12)
ICC_LAG_2003 <- ts(Base_1$ICC_LAG_2003, start = 2002, freq = 12)
IPC_BMV <- ts(Base_1$IPC_BMV, start = 2002, freq = 12)
TDC <- ts(Base_1$TDC, start = 2002, freq = 12)

#****************************************************************************************
# Graficas de clase

# GRAFICA 1
summary(IGAE_2013, digits = 4)

summary(IGAE_PRIM_2013, digits = 4)

png("G1_IGAE.jpg",  width = 900)
# Indicador Global de la Actividad Economica, base 2013
plot(IGAE_2013, type = "l", lwd = 1, col = "red", ylab = "Indice", xlab = "Tiempo", ylim = c(60,160)) 
# Comando que indica a R que, sin borrar la grafica anterior, grafique la siguiente:
par(new = T) 
# Indicador Global de la Actividad Econ?mica, Actividades Primarias, base 2008
plot(IGAE_PRIM_2013, type = "l", lwd = 1, col = "blue", ylab = "Indice", xlab = "Tiempo", ylim = c(60,160))
# Leyenda
legend("topleft", c("IGAE","IGAE Act. Prim."), cex = 0.8, lty = 1:1, col = c("red", "blue"))
par(new = F)
dev.off()

#

# GRAFICA 2
summary(ICC_2003, digits = 4)

summary(ICC_LAG_2003, digits = 4)

png("G2_ICC.jpg",  width = 900)
# Indice de confianza del Consumidor, base enero 2003
plot(ICC_2003, type = "l", lwd = 1, col = "red", ylab = "Indice", xlab = "Tiempo", ylim = c(65,115))
# Comando que indica a R que sin borrar la grafica anterior, grafique la siguiente.
par(new = T) 
# Indice ??Como considera usted la situacion economica del pais hoy en dia comparada con la de hace 12 meses?, base enero 2003
plot(ICC_LAG_2003, type = "l", lwd = 1, col = "blue", ylab = "Indice", xlab = "Tiempo", ylim = c(65,115))
# Leyenda
legend("bottomleft", c("ICC","ICC lag"), cex = 0.8, lty = 1:1, col = c("red", "blue"))
par(new = F)
dev.off()

#

# GRAFICA 3
png("G3_IPC_TDC.jpg",  width = 900)
par(mfrow=c(1,2))
# Indice de Precios y Cotizaciones de la Bolsa Mexicana de Valores
plot(IPC_BMV, type = "l", lwd = 1, col = "red", ylab = "Indice", xlab = "Tiempo", main = "Indice de Precios y Cotizaciones BMV")
# Tipo de Cambio para Solventar Obligaciones en Moneda Extranjera
plot(TDC, type = "l", lwd = 1, col = "blue", ylab = "Pesos X Dolar", xlab = "Tiempo", main = "Tipo de Cambio")
par(mfrow=c(1,1))
dev.off()

#

# GRAFICA 4
# Tansformacion de series: numeros indice respecto del primer mes de la muestra
head(Base_1) # El primer mes de la muestra es enero de 2002

IPC_BMV_I <- 100*IPC_BMV/IPC_BMV[1]
TDC_I <- 100*TDC/TDC[1]

summary(IPC_BMV_I, digits = 4)
summary(TDC_I, digits = 4)

png("G4_IPC_TDC_I.jpg",  width = 900)
# Indice del indice de Precios y Cotizaciones de la Bolsa Mexicana de Valores
plot(IPC_BMV_I, type = "l", lwd = 1, col = "red", ylab = "Indice", xlab = "Tiempo", ylim = c(80,740))
# Comando que indica a R que sin borrar la grafica anterior, grafique la siguiente.
par(new = T)
# Indice del Tipo de Cambio para Solventar Obligaciones en Moneda Extranjera
plot(TDC_I, type = "l", lwd = 1, col = "blue", ylab = "Indice", xlab = "Tiempo", ylim = c(80,740))
# Leyenda
legend("topleft", c("Indice del IPC","Indice del TDC"), cex = 0.8, lty = 1:1, col = c("red", "blue"))
par(new = F)
dev.off()

#

# GRAFICA 5
# Tranformacion de los datos dentro de la funcion PLOT, ejemplo: diferencias logaritmicas
png("G5_DIFF_LOG.jpg")
par(mfrow=c(3,1))
# Indicador Global de la Actividad Econ?mica, base 2008
plot(diff(log(IGAE_2013), lag = 1), type = "l", lwd = 1, col = "red", ylab = "Var. %", xlab = "Tiempo", main = "Indicador Global de la Actividad Economica") 
# Indice de Precios y Cotizaciones de la Bolsa Mexicana de Valores
plot(diff(log(IPC_BMV), lag = 1), type = "l", lwd = 1, col = "red", ylab = "Var. %", xlab = "Tiempo", main = "Indice de Precios y Cotizaciones BMV")
# Tipo de Cambio para Solventar Obligaciones en Moneda Extranjera
plot(diff(log(TDC), lag = 1), type = "l", lwd = 1, col = "blue", ylab = "Pesos X Dolar", xlab = "Tiempo", main = "Tipo de Cambio")
par(mfrow=c(1,1))
dev.off()

# 

save(Base_1, file = "Base_1.RData")

load("Base_1.Rdata")

#