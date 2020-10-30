# 
# Funcion para determinar los rezagos OPTIMOS de un SARIMA(p, i, q)(P, I, Q)m 
# con variables exogenas:
Lag_Opt_SARIMA_Exog <- function(p_max, q_max, P_max, Q_max, X_t, Ex, Z_t){
  # p_max: Rezagos maximos a evaluar del componente AR
  # q_max: Rezagos maximos a evaluar del componente MA
  # P_max: Rezagos maximos a evaluar del componente ARm
  # Q_max: Rezagos maximos a evaluar del componente MAm
  # X_t: Serie de Tiempo modelada
  # Ex: es indicador de que incluye variables exogenas (0 indica que no se incluyen, y 1 que si)
  # Z_t: Vector o Matriz de variables exogenas
  
  if(Ex == 0){
    Criterio_AIC <- matrix(rep(0, 6*(p_max*q_max*(P_max+1)*(Q_max+1))), ncol = 6)
    colnames(Criterio_AIC) <- c("Rezago p", "Rezago q","Rezago P", "Rezago Q", "AIC", "Optimo")
    grupo <- 1
    
    for(i in 1:p_max){
      for(j in 1:q_max){
        for(k in 0:P_max){
          for (m in 0:Q_max){
            Criterio_AIC[grupo,1] <- i
            Criterio_AIC[grupo,2] <- j
            Criterio_AIC[grupo,3] <- k
            Criterio_AIC[grupo,4] <- m
            Criterio_AIC[grupo,5] <- AIC(arima(X_t, order = c(i, 0, j), 
                                               seasonal = c(k, 0, m), method = "ML"))
            grupo = grupo+1
            
          }
        }
      }
    }      
    
    Criterio_AIC[Criterio_AIC[,5] == min(Criterio_AIC[,5]),6] = 1
    
    return(Criterio_AIC)
    #
  } else {
    Criterio_AIC <- matrix(rep(0, 6*(p_max*q_max*(P_max+1)*(Q_max+1))), ncol = 6)
    colnames(Criterio_AIC) <- c("Rezago p", "Rezago q","Rezago P", "Rezago Q", "AIC", "Optimo")
    grupo <- 1
    for(i in 1:p_max){
      for(j in 1:q_max){
        for(k in 0:P_max){
          for(m in 0:Q_max){
            Criterio_AIC[grupo,1] <- i
            Criterio_AIC[grupo,2] <- j
            Criterio_AIC[grupo,3] = k
            Criterio_AIC[grupo,4] <- m
            Criterio_AIC[grupo,5] <- AIC(arima(X_t, order = c(i, 0, j), 
                                               seasonal = c(k, 0, m), 
                                               xreg = Z_t, method = "ML"))
            grupo = grupo+1
            
          }
        }
      }
    }
    Criterio_AIC[Criterio_AIC[,5] == min(Criterio_AIC[,5]),6] = 1
    return(Criterio_AIC)
  }
  
}

