# Compute MA roots 
maroots <- function(object)
{
  if(class(object) != "Arima")
    stop("object must be of class Arima")
    parvec <- object$model$theta
    if(length(parvec) > 0)
      {
      last.nonzero <- max(which(abs(parvec) > 1e-08))
      if (last.nonzero > 0)
      return(structure(list(roots = polyroot(c(1, parvec[1:last.nonzero])),
                            type = "MA"), 
                            class = 'armaroots'))
      }
    return(structure(list(roots = numeric(0), 
                          type = "MA"),
                          class = 'armaroots'))
}

#
