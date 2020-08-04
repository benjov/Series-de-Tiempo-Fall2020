# Series de Tiempo, Agosto de 2020
# Clase 2. Simulacion de un proceso estocastico
#****************************************************************************************
#install.packages("ggplot2")
#install.packages("dplyr")
#
library(ggplot2)
library(dplyr)
#****************************************************************************************
setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I")

getwd()

#****************************************************************************************
# Supongamos un proceso estocastico U_t, el cual es puramente aleatorio, en los siguientes
# terminos:
# i)   E[U_t] = mu = 0 
# ii)  Var[U_t] = E[(U_t - mu)^2] = E[(U_t)^2] = sigma^2
# ii)  Cov[U_t,U_s] = E[(U_t - mu)(U_s - mu)] = E[U_t U_s] = 0, para todo t distinto de s

#****************************************************************************************
# A. Utilizaremos una funcion de numeros aleatorios: 
#-> rnorm

args(rnorm)

# Supongamos E[U_t] = 0 y Var[U_t] = 4

rnorm(10, mean = 0, sd = 2)

# Importante (para Simulaciones que sean replicables):
#set.seed(10101) # Es una constate (la semilla) que fija la serie de numeros aleatorios
Z_t <- rnorm(1000, mean = 0, sd = 2)

mean(Z_t)

sd(Z_t)

hist(Z_t, 
     main = "Histograma del proceso Z_t, Normal(0, 2)", 
     xlab = "Z_t", 
     ylab = "Frecuencia")

# ¿Cómo guardar la gráfica?
png("G_Z_t.jpg",  width = 900)

hist(Z_t, 
     main = "Histograma del proceso Z_t, Normal(0, 2)", 
     xlab = "Z_t", 
     ylab = "Frecuencia")

dev.off()

#****************************************************************************************
# B. Generamos una función que hace un proceso de cualquier dimensión, para el caso del
# lanzamiento de una moneda y un proceso estocástico del tipo X_t = X_t-1 + U_t
# Una Función tiene una estructura:
# function( lista de argumentos -- pede ser vacio )
#   {
#   Cálculos, prpcedimientos, etc.
#   return( objeto de resultados ) -- puede ser opcional
#   }

# La siguiente funcion necesita DOS argumentos

Moneda <- function(R, Time){
  # R: es el conjunto del cual se extrae la muestra - espacio muestral
  # Time: es el tamaño de la serie resultante
  # S: es el tamaño de los subconjuntos generados
  U_t <- replicate(Time, 
                   sample(R, size = 1, replace = TRUE) )
  # Esta es una funcion que genera un vector de resultados aleatorios de los posibles en
  # el espacio muestral, tomando muestras de tamaño 1
  return(U_t)
}

#****************************************************************************************
# Resutados: 
# Inputs / argumentos de la funcion, para el caso de lanzar una moneda

Resultados <- c(-1, 1)
#
Periodos <- 10000

# Creamos un data frame de nombre "U" con una columna llamada "Tiempo"
U <- data.frame(Tiempo = c(1:Periodos))

# Agregamos al data frame una columna "U_t" utilizando nuestra función de lanzamiento de
# una moneda

U$U_t <- Moneda( Resultados, Periodos )

#
mu = mean(U$U_t)
mu

#
S2 = var(U$U_t)
S2

# Agregamos al data frame una columna SU_t que llenamos de ceros (0)
U$SU_t <- replicate(Periodos, 0)

View(U)

# El siguiente ciclo recorre el data frame para ir acumulando los valores de U_t
# y colocando el valor acumulado en SU_t
for(i in 1:Periodos){
  U$SU_t[i] <- sum( U$U_t[1:i] )
}

View(U)

# Graficamos los resultados:

# GRÁFICA
ggplot(data = U, aes( x = Tiempo, y = SU_t) ) + 
  geom_line(size = 0.5, color = "darkblue") +
  #geom_point(size = 1.0, color = "darkblue") + 
  #theme_bw() + 
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  guides(col = guide_legend(nrow = 1, byrow = TRUE)) + 
  xlab("Tiempo") + 
  ylab("Xt") + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0)) + 
  theme(plot.subtitle = element_text(size = 10, hjust = 0)) + 
  theme(plot.caption = element_text(size = 10, hjust = 0)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(
    title = "Gráfica de la serie de tiempo Xt",
    subtitle = "Valores en pesos",
    caption = "Fuente: Elaboración propia. \nNotas: Simulación del lanzamiento de una moneda."
  )

ggsave("Lanzamiento_Moneda.png", width = 30, height = 15, units = "cm")

#

#****************************************************************************************
# C. Solucion a traves de una funcion

# Utilizaremos una función guardada en un archivo a parte
# Llamamos a la función:
source("Caminata.R")

# Definimos argumentos de la función
Opciones <- c(-1, 1)
#
Soporte <- 10000

# Vamos a réplicar el proceso con estos parámetros
Rango <- 200
#
Caminos <- 10

#

for(i in 1:Caminos){
  TT <- data.matrix(data.frame(Caminata(Opciones, Soporte)[1]))
  #
  G_t <- data.matrix(data.frame(Caminata(Opciones, Soporte)[2]))
  #
  plot(TT, G_t, col = "blue", type = "l", ylab = "Ganancias", xlab = "Tiempo", ylim = c(-Rango,Rango))
  #
  par(new = TRUE)
  #
  i <- i +1
}
#
par(new = FALSE)

#****************************************************************************************
#

