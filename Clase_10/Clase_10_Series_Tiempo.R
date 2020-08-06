# Series de Tiempo, Septiembre de 2020
# Clase 10. Aplicacion Forecast de ARIMA(p, d, q) y ARMA(p, q)
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_10")
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
# Estimacion del proceso ARIMA(p, 1, q):

ARMA_Ex_DLPax_Nal_2 <- arima(DLPax_Nal, order = c(4, 0, 6),
                             xreg = cbind(D_Ene, D_Feb, D_Jul, D_Dic, D_May2009, D_Jun2009),
                             method = "ML")

ARMA_Ex_DLPax_Nal_2

#****************************************************************************************
# Forecast:

# Datos Exogenos:
Predict_Datos <- read_excel("Predict_Base_Transporte_ARIMA.xlsx", sheet = "Datos", col_names = TRUE)

# Los datos importados son:
# D_Sep2017: Dummy que toma el valor de 1 en septiembre de 2017
# D_Ene: Dummy que toma el valor de 1 en todos los eneros
# D_Feb: Dummy que toma el valor de 1 en todos los febreros
# D_Jul: Dummy que toma el valor de 1 en todos los julios
# D_Dic: Dummy que toma el valor de 1 en todos los diciembres
# D_May2009: Dummy que toma el valor de 1 en mayo de 2009
# D_Jun2009:Dummy que toma el valor de 1 en junio de 2009

# Conversion a series de tiempo: 

D_Ene_f <- ts(Predict_Datos$D_Ene, 
              start = c(2019, 7), 
              freq = 12)

D_Feb_f <- ts(Predict_Datos$D_Feb, 
              start = c(2019, 7), 
              freq = 12)

D_Jul_f <- ts(Predict_Datos$D_Jul, 
              start = c(2019, 7), 
              freq = 12)

D_Dic_f <- ts(Predict_Datos$D_Dic, 
              start = c(2019, 7), 
              freq = 12)

D_May2009_f <- ts(Predict_Datos$D_May2009, 
                  start = c(2019, 7), 
                  freq = 12)

D_Jun2009_f <- ts(Predict_Datos$D_Jun2009, 
                  start = c(2019, 7), 
                  freq = 12)

# Prediccion 1 año:

predict(ARMA_Ex_DLPax_Nal_2, n.ahead = 24, 
        newxreg = cbind(D_Ene_f, D_Feb_f, D_Jul_f, D_Dic_f, D_May2009_f, D_Jun2009_f))

DLPax_Nal_f <- predict(ARMA_Ex_DLPax_Nal_2, n.ahead = 24, 
                      newxreg = cbind(D_Ene_f, D_Feb_f, D_Jul_f, D_Dic_f, D_May2009_f, D_Jun2009_f))

names(DLPax_Nal_f)

# Prediccion de la serie original:

Pronostico_Arima <- read_excel("Pronostico_Arima.xlsx", sheet = "Datos", col_names = TRUE)

Pronostico_Arima$Pax_Nal_f <- Pronostico_Arima$Pax_Nal

for(i in 1:24){
  Pronostico_Arima$Pax_Nal_f[234 + i] <- 
    Pronostico_Arima$Pax_Nal_f[234 + i - 1]*(1 + DLPax_Nal_f$pred[i])
}

# Grafica

ggplot(data = Pronostico_Arima, aes(x = Periodo)) +
  geom_line(aes(y = Pax_Nal_f, color = "Pax_Nal_f")) +
  geom_line(aes(y = Pax_Nal, color = "Pax_Nal")) +
  scale_color_brewer(type = "qual", palette = 2) +
  #theme_bw() + 
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  guides(col = guide_legend(nrow = 1, byrow = TRUE)) + 
  xlab("Tiempo") + 
  ylab("Pasajeros") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0)) + 
  theme(plot.subtitle = element_text(size = 10, hjust = 0)) + 
  theme(plot.caption = element_text(size = 10, hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(
    title = "Pasajeros en vuelos nacionales (Salidas)",
    subtitle = "(Ene-2000 a Jun-2019)",
    caption = "Fuente: Elaboración propia"
  )

ggsave("Pax_Nal_f.png", width = 20, height = 15, units = "cm")

#
