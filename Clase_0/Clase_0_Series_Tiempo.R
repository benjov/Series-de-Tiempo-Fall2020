# Series de Tiempo, Agosto de 2020
# Primeros Códigos. Introduccion

#****************************************************************************************
# Reglas importantes:
#  1. R es case sensitive, lo que significa que __distingue entre minusculas y mayusculas__
#  2. Todo lo que se escribe despues del simbolo de "#", __R lo considera un comentario__
#  3. Para __ejecutar una linea__ de comando se utiliza "Control + Enter"
#  4. Para __limpiar la Consola__ se utiliza "Control + L"
#  5. Utilizar _";"_ funciona para realizar más de una tarea en la misma línea de código
#  6. En R existen un conjunto de __palabras reservadas__, como lo son: break, for, function, if, in, next, repeat, return, while, True, False, y algunas otras
#  
#****************************************************************************************

## Clases de objetos

class("a")
class("R">"Python"); "R" > "Python"; 5<2
class(2); class(as.integer(2)); class("2")

#Asignación e impresión de objetos 

ObjetoGuardado <- c(1,2,3,5,7,11,13,17)
print(ObjetoGuardado)
ObjetoGuardado

# Operaciones: +, -, *, /, ^, log, exp, ...

ObjetoGuardado+5
ObjetoGuardado*5
ObjetoGuardado**(1/2)
log(ObjetoGuardado)
exp(ObjetoGuardado)
round(exp(ObjetoGuardado), 2)

## Clases de objetos (Vectores y listas)

# Para revisar cuantos elementos hemos escrito:

ls() # "ls" lista todos los elementos contenidos en Global Enviroment

# ## Clases de objetos (Vectores y listas)

class(ObjetoGuardado)

I <- c(1, 1, 1, 1, 1, 1,1,1)

ObjetoGuardado %*% I # Producto punto entre vectores

ListaGuardada <- list(c("Omar", "José"),c(24,29),c(TRUE, FALSE))
ListaGuardada

class(ListaGuardada)

## Clases de objetos (Matrix y DataFrame)
M_object <-matrix(ObjetoGuardado, nrow = 3, ncol=2) #Generamos una matriz a partir del vector
diagonal <- diag(3) #Generamos una matríz identidad de 3*3 
M_object%*%diagonal #¿Por qué no imprime el resultado?
t(M_object)%*%diagonal #¿Por qué esta sí funciona?
solve(matrix(ObjetoGuardado, nrow = 3)) #Matriz inversa

miData = data.frame(ListaGuardada) #Generamos un DataFrame a partir del objeto clase lista
names(miData) = c("Nombre", "Edad", "PEA")#Le damos nombre a las columnas 
miData #Imprimimos

## Clases de objetos (Funciones)

### Estadisticas, suma, media, desviación estándar y muestreo:

sum(ObjetoGuardado)
mean(ObjetoGuardado)
sd(ObjetoGuardado)
###Las funciones necesitan ARGUMENTOS, en el caos siguiente el argumento es el tamaño de la muestra
sample(ObjetoGuardado,3) 
###Para conocer los argumentos d euna función, se puede utilizar args()
args(sample)

###Funciones creadas
m_podada <- function(x, n){
  N = length(x)
  x = x[(n+1):(N-n)]
  sum(x)/length(x)
}
###probar funcion
m_podada(ObjetoGuardado,2)
###Como alternativa podemos usar librerias que contengan un proceso como el que ocuparemos 
install.packages("AER")
library(AER)


#****************************************************************************************
# Graficas

plot(ObjetoGuardado)
plot(C)

#****************************************************************************************
# Caso Práctico (Cálculo de riesgo sistémico de AMZN)

### Librería para descargar series financieras y graficarlas
install.packages("quantmod")
install.packages("highcharter")
install.packages("ggplot2")
library(quantmod)
library(highcharter)
library(ggplot2)
options("getSymbols.warning4.0"=FALSE)

### Descargamos la serie de las acciones de Amazon 
getSymbols("AMZN")
head(AMZN, 2)
### Gráfico
hchart(AMZN)

### Descargamos la serie de las acciones de Amazon 
getSymbols("NDAQ")
head(AMZN, 2)
### Gráfico
hchart(NDAQ)

## Series en Diferencias

lnAMZN <- log(AMZN$AMZN.Adjusted)
diffAMZN <- lnAMZN- Lag(lnAMZN)
lnNDAQ <- log(NDAQ$NDAQ.Adjusted)
diffNDAQ <- lnNDAQ - Lag(lnNDAQ)


#Gráfico de series en diferencia 
par(mfrow=c(1,2))
plot(diffAMZN)
plot(diffNDAQ)

## construir scatter plot de ambas series 
class(AMZN)

AMZNN<-as.numeric(diffAMZN); NASDAQQ<-as.numeric(diffNDAQ)
### Análisis exploratorio de datos
qplot(x=NASDAQQ, y=AMZNN)+
  geom_smooth()
### Análisis final
ggplot()+
  geom_point(aes(x=NASDAQQ, y=AMZNN, alpha=0.05))+
  geom_smooth(aes(x=NASDAQQ, y=AMZNN), se=FALSE)+
  labs(title="Gráfico de dispersión AMZN~NDAQ")

### Para generar OLS
reg1 <- lm(AMZNN~NASDAQQ)
summary(reg1)
