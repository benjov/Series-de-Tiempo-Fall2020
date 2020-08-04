#*********************************************************************************************
# Funcion CAMINATA ALEATORIA:
Caminata <- function(R, Time){
  # R: es el conjunto del cual se extrae la muestra - espacio muestral
  # Time: es el tamaño de la serie resultante
  #
  U_t <- replicate(Time, 
                   sample(R, size = 1, replace = TRUE) )
  # Esta es una funcion que genera un vector de resultados aleatorios de los posibles en
  # el espacio muestral, tomando muestras de tama??o 1
  #
  Tiempo <- c(1:Time)
  #
  SU_t <- replicate(Time, 0)
  #
  
  for(i in 1:Time){
    SU_t[i] <- sum(U_t[1:i])
  }
  #
  return(list(Tiempo, SU_t))
}
#
#*********************************************************************************************
