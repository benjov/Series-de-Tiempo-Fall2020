# Series de Tiempo, Agosto de 2020
# Primeros Códigos. Introduccion

#****************************************************************************************
# Reglas importantes:
# 1. R es case sensitive - por lo que si distingue entre minusculas y mayusculas
# 2. Todo lo que se escribe despues del simbolo de "#", R lo considera como comentarios
# 3. Para ejecutar una linea de comando se utiliza "Control + Enter"
# 4. Para limpiar la Consola se utiliza "Control + L"
#****************************************************************************************

# Imprime la secuencia del 100 al 130

100:130

# Asignacion a una varaible

A <- 1:100

# Operaciones: +, -, *, /, ^, log, exp, ...

5 + 3

a <- 5 + 3

a * 10

# Asignacion e impresion de variables

Dado <- 1:6

Dado

# Para revisar cuantos elementos hemos escrito:

ls() # "ls" lista todos los elementos contenidos en Global Enviroment

# Vectores, definicion y operaciones:

I <- c(1, 1, 1, 1, 1, 1)

II <- rep(1, 6)

Dado %*% I # Producto punto entre vectores

# Estadisticas, media, redondeo y muestreo:

mean(Dado)

pi # Algunas constantes: pi, exp(1), etc.

sample(1:100, size = 3)

sample(C <- 1:100, size = 3)

sample(C, size = 10)

args(sample) # Cuando se tenga duda de cuales son los argumentos de una funcion

sample(C, size = 10, replace = TRUE)

sample(C, size = 10, replace = FALSE)

# Cual es la diferencia en un muestreo con reemplazo y en uno sin reemplazo????
# Un ejemplo....

sample(1:10, size = 10, replace = TRUE)

sample(1:10, size = 10, replace = FALSE)

args(round)

round(0.123456789, digits = 3)

round(0.123456789, digits = 4)

round(0.123456789, digits = 5)

round(0.123456789, digits = 6)

round(mean(pi), 4)

#****************************************************************************************
# Funciones, en ocasiones es relevante usar funciones...
# Funcion de lanzamiento de un Dado!!!

# Caso 1. Funcion que NO requiere de ingresarle parametros

roll <- function(){
  Dado2 <- 1:6 # Definimos el objeto Dado
  DDado <- sample(Dado2, size = 2, replace = TRUE) # Simulamos el lanzamiento de 2 dados
  sum(DDado) # Suma del valor
}

roll

roll()

# Caso 2. Funcion que SI requiere de ingresarle parametros

roll2 <- function(X){
  DDado <- sample(X, size = 2, replace = TRUE) # Simulamos el lanzamiento de 2 dado
  sum(DDado)
}

roll2(Dado)

#****************************************************************************************
# Graficas

x <- -10:10

y <- x^3

plot(x, y)

plot(x, y, type = "o", col = "red", ylab = "y = f(x)", xlab = "x")

plot(x, y, type = "l", col = "blue", ylab = "y = f(x)", xlab = "x")

#****************************************************************************************
# Coloquemos atributos a las variables:

Dado

attributes(Dado)

names(Dado)

names(Dado) <- c("Uno", "Dos", "Tres", "Cuatro", "Cinco", "Seis")

names(Dado)

attributes(Dado)

Dado

names(Dado) <- NULL # Modificamos los nombres, los eliminamos

attributes(Dado)

Dado

#****************************************************************************************
# Matrices y Dimension:

C # Retomamos una variable creada anteriormente

dim(C)

dim(C) <- c(10, 10)

dim(C)

C

M <- matrix(1:9, nrow = 3)

M

A <- matrix(1:10, nrow = 5, byrow = TRUE)

A

B <- matrix(rep(1, 10), nrow = 5)

B

t(A) # Trasponer

t(A)%*%B # Multiplicacion de matrices

A%*%B # ¿Por qué no se puede hacer esta operacion?

diag(M)

C <- matrix(c(2, 4, 3, 1, 5, 7), nrow = 2, ncol = 3, byrow = TRUE)

C

D <- matrix(c(2, 4, 3, 1), nrow = 2, ncol = 2, byrow = TRUE)

D

solve(D) # Matriz inversa

solve(D) %*% D

M

M[2, 3] # Seleccion de valores particulares de la matriz

M[3, ]

M[ , 2]

M[c(1, 2), ]

M[ , c(2, 3)]

#****************************************************************************************
# DATA FRAMES:
# Un data Frame es una version bidimencional de las listas. Es decir, contienen titulos 
# o nombres de cada variable y pueden contener valores numericos, categoricos o de texto 
# en dichas variables

DF_Cartas <- data.frame(Carta = c("As", "Dos", "Seis"), 
                        Palo = c("Diamante", "Espada", "Diamante"), 
                        Valor = c(1, 2, 3))

DF_Cartas

View(DF_Cartas)

DF_Cartas$Valor # Seleccionamos una columna del Data Frame

#