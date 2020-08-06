# Series de Tiempo, Septiembre de 2020
# Clase 4. Simulación de un proceso AR(1)
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
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_4")
getwd()

#****************************************************************************************
# Definamos la serie de tiempo
# El proceso AR(1) es: X_t = a0 + a1*X_t-1 + U_t
# 
#****************************************************************************************
# Parametros:
delta <- 5; alpha <- 0.9; X_0 <- (delta/(1 - alpha)); T <- 1000

# Definimos un data frame para almacenar el proceso, agregamos una columna para el tiempo
X_t <- data.frame(Tiempo = c(0:T))
  
#****************************************************************************************
# Parte estocastica de la serie de tiempo:

#set.seed(12345)

# Agregamos un término estocástico al data frame
X_t$U_t <- rnorm(T+1, mean = 0, sd = 4)

# GRÁFICA
ggplot(data = X_t, aes(x = Tiempo, y = U_t)) + 
  geom_line(size = 0.5, color = "darkblue") +
  #theme_bw() + 
  xlab("Tiempo") + 
  ylab("U_t") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0)) + 
  theme(plot.subtitle = element_text(size = 10, hjust = 0)) + 
  theme(plot.caption = element_text(size = 10, hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(
    title = "Proceso 'puramente' aleatorio",
    subtitle = "Distribución Normal, media = 0, desviación estándar = 4",
    caption = "Fuente: Elaboración propia."
  )

#****************************************************************************************
# Trayectoria del proceso AR(1) y de la solucion del mismo:
# X_t = (delta/(1 - alpha)) + SUM(alpha^j*U_t-j)

# Agregamos columnas con NA's para un proceso teorico y uno real
X_t$X_t <- NA
X_t$XR_t <- NA

# La serie teórica inicia en un valor inicial X_0
X_t$X_t[1] <- X_0

# La serie real inicia en un valor inicial X_0
X_t$XR_t[1] <- X_0

# Agregamos una columna para la función de Autocorrelación teórica:
X_t$rho <-NA

#****************************************************************************************
# Construcción de dos procesos: uno 'real' y uno 'estimado' o teórico y autocorrelación
for (i in 2:(T + 1)) {
  # Real:
  X_t$XR_t[i] = delta + alpha*X_t$XR_t[i-1] + X_t$U_t[i-1]
  
  # Teórico:
  X_t$X_t[i] = X_t$X_t[i-1] + (alpha^(i-1))*X_t$U_t[i-1]
  
  # Autocorrelación:
  X_t$rho[i-1] = alpha^(i-1)
}

#****************************************************************************************

# GRÁFICA
ggplot(data = X_t, aes(x = Tiempo, y = X_t)) + 
  geom_line(size = 0.5, color = "darkblue") +
  #theme_bw() + 
  xlab("Tiempo") + 
  ylab("X_t") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0)) + 
  theme(plot.subtitle = element_text(size = 10, hjust = 0)) + 
  theme(plot.caption = element_text(size = 10, hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(
    title = "Comportamiento del Proceso Teórico",
    subtitle = "Con un error con Distribución Normal (media = 0, desviación estándar = 4)",
    caption = "Fuente: Elaboración propia."
  )

ggsave("G_AR_1_Teo.png", width = 20, height = 10, units = "cm")

#****************************************************************************************

# GRÁFICA
ggplot(data = X_t, aes(x = Tiempo, y = XR_t)) + 
  geom_line(size = 0.5, color = "darkred") +
  #theme_bw() + 
  xlab("Tiempo") + 
  ylab("X_t") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0)) + 
  theme(plot.subtitle = element_text(size = 10, hjust = 0)) + 
  theme(plot.caption = element_text(size = 10, hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(
    title = "Comportamiento del Proceso Real ('Estimado')",
    subtitle = "Con un error con Distribución Normal (media = 0, desviación estándar = 4)",
    caption = "Fuente: Elaboración propia."
  )

ggsave("G_AR_1_Real.png", width = 20, height = 10, units = "cm")

#****************************************************************************************

# GRÁFICA
ggplot(data = X_t, aes(x = Tiempo)) +
  geom_line(aes(y = XR_t), size = 0.5, color = "darkred") +
  geom_line(aes(y = X_t), size = 0.5, color = "darkblue") +
  #theme_bw() + 
  xlab("Tiempo") + 
  ylab("X_t") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0)) + 
  theme(plot.subtitle = element_text(size = 10, hjust = 0)) + 
  theme(plot.caption = element_text(size = 10, hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(
    title = "Comportamiento de los Procesos Real y Teórico",
    subtitle = "Con un error con Distribución Normal (media = 0, desviación estándar = 4)",
    caption = "Fuente: Elaboración propia."
  )

ggsave("G_AR_1_Comb.png", width = 20, height = 10, units = "cm")

#****************************************************************************************

# GRÁFICA
acf(X_t$XR_t, lag.max = 30, col = "blue", 
    ylab = "Autocorrelacion",
    xlab="Rezagos", 
    main="Funcion de Autocorrelacion Real")

# GRÁFICA
barplot(X_t$rho[1:30], names.arg = c(1:30), col = "blue", border="blue", density = c(10,20), 
        ylab = "Autocorrelacion", 
        xlab="Rezagos", 
        main="Funcion de Autocorrelacion Teórica")

#
