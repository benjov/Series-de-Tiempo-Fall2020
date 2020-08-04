# Series de Tiempo, Agosto de 2020
# Clase 3. Autocorrelacion y otras pruebas sobre los datos
#****************************************************************************************
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readxl")
# 
library(ggplot2)
library(dplyr)
library(readxl)
# 
#****************************************************************************************
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I")
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

View(Datos)

names(Datos)

head(Datos)

str(Datos)

dim(Datos)

Datos[ , 2]

Datos[5 , ]

Datos[c(2:197) , 2]

Datos[ , c(2:6)]

Datos[ ,c(2, 3, 4, 5, 6)]

Datos[c("Pax_Nal", "Pax_Int", "Vue_Nal", "Vue_Int")]

#****************************************************************************************
# Tabla de estadísticas decriptivas
# 
summary( Datos[ ,c(2:6)] )

Resumen1 <- summary(Datos[ ,c(2:6)])

Resumen1

#****************************************************************************************
# Conversion a series de tiempo con la función ts --time series:
# 

# Una serie - univariado:
Pax_Metro <- ts(Datos$Pax_Metro, 
                start = 2000, 
                freq = 12)

# Varias series - multivariado:
Dat_Aereo <- ts(Datos[c("Pax_Nal", "Pax_Int", "Vue_Nal", "Vue_Int")], 
                start = 2000, 
                freq = 12)

#****************************************************************************************
# Graficas:

plot(Pax_Metro, 
     col = "darkblue", 
     xlab = "Tiempo",  
     type = "l", lwd = 2,
     ylab = "Millones de personas",
     main = "Pasajeros Transportados en el Metro de la CDMX \n(Ene-2000 a Jun-2019)",
     sub = "Fuente: Elaboración propia con información del INEGI, https://www.inegi.org.mx/app/indicadores/?tm=0&t=1090")

#****************************************************************************************

ggplot(data = Datos, aes(x = Periodo, y = Pax_Metro)) + 
  geom_line(size = 0.5, color = "darkblue") +
  #geom_point(size = 1.0, color = "darkblue") + 
  #theme_bw() + 
  xlab("Tiempo") + 
  ylab("Millones de pasajeros") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0)) + 
  theme(plot.subtitle = element_text(size = 10, hjust = 0)) + 
  theme(plot.caption = element_text(size = 10, hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(
    title = "Pasajeros Transportados en el Metro de la CDMX",
    subtitle = "(Ene-2000 a Jun-2019)",
    caption = "Fuente: Elaboración propia con información del INEGI, \nhttps://www.inegi.org.mx/app/indicadores/?tm=0&t=1090"
  )
#
ggsave("Pax_Metro.png", width = 20, height = 15, units = "cm")

#****************************************************************************************

ggplot(data = Datos, aes(x = Periodo)) +
  geom_line(aes(y = Pax_Nal, color = "Pax_Nal")) +
  geom_line(aes(y = Pax_Int, color = "Pax_Int")) +
  scale_color_brewer(type = "qual", palette = 6) +
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
    title = "Pasajeros en vuelos nacionales e internacionales (Salidas)",
    subtitle = "(Ene-2000 a Jun-2019)",
    caption = "Fuente: Elaboración propia con información del INEGI, \nhttps://www.inegi.org.mx/app/indicadores/?tm=0&t=1090"
  )
#

#****************************************************************************************
# EJERCICIO -- COMPLETAR:
ggplot(data = Datos, aes(x = Periodo)) +
  geom_line(aes(y = Pax_Nal, color = )) +
  geom_line(aes(y = Pax_Int, color = )) +
  scale_color_brewer(type = "qual", palette = 6) +
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
    title = "Número de vuelos nacionales e internacionales (Salidas)",
    subtitle = "(Ene-2000 a Jun-2019)",
    caption = "Fuente: Elaboración propia con información del INEGI, \nhttps://www.inegi.org.mx/app/indicadores/?tm=0&t=1090"
  )
#

#****************************************************************************************
# Tabla de estadisticas de caracterización de las Series de Tiempo

Tabla_Pax_Metro <- data.frame(matrix(c("Media", 
                                      "Varianza", 
                                      "Covarianza (t, t - 1)", 
                                      "Covarianza (t, t - 2)", 
                                      "Covarianza (t, t - 3)", 
                                      "Autocorrelacion(1)", 
                                      "Autocorrelacion(2)", 
                                      "Autocorrelacion(2)", 
                                      "Q(1)", 
                                      "Q(3)*"), 
                                    nrow = 10, 
                                    ncol = 1) )

#
Tabla_Pax_Metro

names(Tabla_Pax_Metro)

names(Tabla_Pax_Metro) <- c("Estadística")

Tabla_Pax_Metro

Tabla_Pax_Metro$Valor <- 0

Tabla_Pax_Metro

# Llenado de la tabla:
#
Tabla_Pax_Metro[1, 2] <- round(mean(Pax_Metro), digits = 2)
Tabla_Pax_Metro[2, 2] <- round(var(Pax_Metro), digits = 2)
Tabla_Pax_Metro[3, 2] <- round(cov(Pax_Metro[2:234], Pax_Metro[1:233]), digits = 2)
Tabla_Pax_Metro[4, 2] <- round(cov(Pax_Metro[3:234], Pax_Metro[1:232]), digits = 2)
Tabla_Pax_Metro[5, 2] <- round(cov(Pax_Metro[4:234], Pax_Metro[1:231]), digits = 2)
Tabla_Pax_Metro[6, 2] <- round(Tabla_Pax_Metro[3, 2]/Tabla_Pax_Metro[2, 2], digits = 4)
Tabla_Pax_Metro[7, 2] <- round(Tabla_Pax_Metro[4, 2]/Tabla_Pax_Metro[2, 2], digits = 4)
Tabla_Pax_Metro[8, 2] <- round(Tabla_Pax_Metro[5, 2]/Tabla_Pax_Metro[2, 2], digits = 4)
Tabla_Pax_Metro[9, 2] <- 234*(Tabla_Pax_Metro[6, 2]^2)
Tabla_Pax_Metro[10, 2] <- 234*(Tabla_Pax_Metro[6, 2]^2 +
                               Tabla_Pax_Metro[7, 2]^2 +
                               Tabla_Pax_Metro[8, 2]^2)

#
Tabla_Pax_Metro

#****************************************************************************************
# Gráfica de la Funcion de Autocorrelacion:

acf(Pax_Metro, 
    lag.max = 150, 
    xlab = 'Resagos k en meses', 
    main="Funcion de Autocorrelacion del número de pasajeros del metro")

#
