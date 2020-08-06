# Compute AR roots 
arroots <- function(object)
  {
    if(class(object) != "Arima" & class(object) != "ar")
      stop("Object must be of class ARIMA or AR")
    if(class(object) == "Arima")
      parvec <- object$model$phi
    else
      parvec <- object$ar
    if(length(parvec) > 0)
    {
      last.nonzero <- max(which(abs(parvec) > 1e-08))
      if (last.nonzero > 0)
        return(structure(list(roots = polyroot(c(1, -parvec[1:last.nonzero])), 
                         type = "AR"), 
                         class='armaroots'))
    }
    return(structure(list(roots = numeric(0), type="AR"), class = 'armaroots'))
  }

#
