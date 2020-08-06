# 
# Funcion para determinar los rezagos OPTIMOS de un ARIMA(p, i, q) con variables exogenas:
  Lag_Opt_ARIMA_Exog <- function(p_max, q_max, X_t, Ex, Z_t){
    # p_max: Rezagos maximos a evaluar del componente AR
    # q_max: Rezagos maximos a evaluar del componente MA
    # X_t: Serie de Tiempo modelada
    # Ex: es indicador de que incluye variables exogenas (0 indica que no se incluyen, y 1 que si)
    # Z_t: Vector o Matriz de variables exogenas
    
    if(Ex == 0){
      Criterio_AIC <- matrix(rep(0, 4*(p_max*q_max)), ncol = 4)
      colnames(Criterio_AIC) <- c("Rezago p", "Rezago q", "AIC", "Optimo")
      grupo <- 0
      for(i in 1:p_max){
        for(j in (1 + grupo):(q_max + grupo)){
          Criterio_AIC[j,1] <- i
          Criterio_AIC[j,2] <- j - grupo
          Criterio_AIC[j,3] <- AIC(arima(X_t, order = c(i, 0, (j - grupo)), method = "ML"))
        }
        grupo <- grupo + q_max
      }
      Optimo <- min(Criterio_AIC[1:(p_max*q_max),3])
      for(i in 1:(p_max*q_max)){
        if(Criterio_AIC[i,3] == Optimo){
          Criterio_AIC[i,4] <- 1
        }
      }
      return(Criterio_AIC)
#
    } else {
      Criterio_AIC <- matrix(rep(0, 4*(p_max*q_max)), ncol = 4)
      colnames(Criterio_AIC) <- c("Rezago p", "Rezago q", "AIC", "Optimo")
      grupo <- 0
      for(i in 1:p_max){
        for(j in (1 + grupo):(q_max + grupo)){
          Criterio_AIC[j,1] <- i
          Criterio_AIC[j,2] <- j - grupo
          Criterio_AIC[j,3] <- AIC(arima(X_t, order = c(i, 0, (j - grupo)), xreg = Z_t, method = "ML"))
        }
        grupo <- grupo + q_max
      }
      Optimo <- min(Criterio_AIC[1:(p_max*q_max),3])
      for(i in 1:(p_max*q_max)){
        if(Criterio_AIC[i,3] == Optimo){
          Criterio_AIC[i,4] <- 1
        }
      }
      return(Criterio_AIC)
    }
  }
