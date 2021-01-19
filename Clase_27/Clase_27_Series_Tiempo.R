# Series de Tiempo, Enero de 2020
# Clase 27. Time-Varying Coefficient Linear Regression
#           Single equation models: tvLM and tvAR
# Source: https://icasas.github.io/tvReg/articles/tvReg.html
#****************************************************************************************
#install.packages("tvReg")

library(Matrix)
library(plm)
library(tvReg)

# Example: tvReg
# Simulemos un proceso lineal que tenga coeficiente que cambian con el tiempo
#****************************************************************************************
# Data:

set.seed(100)

#
tau <- seq(1:1000)/1000

#
beta <- data.frame(beta1 = sin(2 * pi * tau), beta2 = 2 * tau)

plot(beta$beta1, type = 'l')

plot(beta$beta2, type = 'l')

#
X1 <- rnorm(1000)
X2 <- rchisq(1000, df = 4)
error <- rt(1000, df = 10)

#
y <- apply(cbind(X1, X2) * beta, 1, sum) + error # X * Beta + Error
data <- data.frame(y = y, X1 = X1, X2 = X2)

#
#****************************************************************************************
# Estimamos los coeficientes usando un modelo lineal y un TVP para comparar

coef.lm <- stats::lm(y ~ 0 + X1 + X2, data = data)$coef
coef.lm

model.tvLM <- tvLM(y ~ 0 + X1 + X2, data = data)
model.tvLM

## Plot the estimates of Beta1

plot(tau, beta[, 1], type = "l", main = "", 
     ylab = expression(beta[1]), 
     xlab = expression(tau), 
     ylim = range(beta[, 1], model.tvLM$tvcoef[, 1]),
     col = 1)
abline(h = coef.lm[1], 
       col = 2)
lines(tau, model.tvLM$coefficients[, 1], 
      col = 4)
legend("topright", c(expression(beta[1]), "lm", "tvlm"), 
       col = c(1, 2, 4), bty = "n", 
       lty = 1, cex = 0.8)

#
#****************************************************************************************
# Intervalo de Confianza al 90% de los coeficientes TVP
model.tvLM.90 <- confint(model.tvLM, level = 0.9, runs = 50)

plot(model.tvLM.90)

# Example: tvAR
# Simulemos un proceso AR(p) tenga coeficiente que cambian con el tiempo
#****************************************************************************************
# Data:

#
tt <- (1:1000)/1000

beta <- cbind(0.5 * cos(2 * pi * tt), (tt - 0.5)^2)

y <- numeric(1000)
y[1] <- 0.5
y[2] <- -0.2

# y(t) = beta1(t) y(t-1) + beta2(t) y(t-2) + u_t

for (t in 3:1000) {
  y[t] <- y[(t - 1):(t - 2)] %*% beta[t, ] + rnorm(1)
}

Y <- tail(y, 500)

#
#****************************************************************************************
# Coefficient estimates 

model.ar.2p <- ar.ols(Y, aic = FALSE, 
                      order = 2, 
                      intercept = FALSE, 
                      demean = FALSE)

model.tvAR.2p <- tvAR(Y, p = 2, 
                      type = "none", 
                      est = "ll")

#
#****************************************************************************************
# Intervalo de Confianza al 80% de los coeficientes TVP

model.tvAR.80 <- confint(model.tvAR.2p, 
                         tboot = "wild2", 
                         level = 0.8, runs = 50)

plot(model.tvAR.80)





# Import the data 
data(traffic)
names(traffic)

# OLS regression 
olsLVS = lm(NDead ~ Temp + Prec, data = traffic)

summary(olsLVS)

#
#****************************************************************************************
# Markov Switching
?msmFit

# MS (k is number of regimes, 6 is for means of 5 variables
# + 1 for volatility)
msLVS = msmFit(olsLVS, k = 2, sw = rep(TRUE, 4))

summary(msLVS)

intervals(msLVS)

#
#****************************************************************************************
# Markov Switching: Plotting probabilities

?plotProb

plotProb(msLVS, which = 1)

plotProb(msLVS, which=2)

#
#****************************************************************************************
#Diagnostics for Markov Switching

?plotDiag

plotDiag(msLVS, regime = 1, which = 1)

plotDiag(msLVS, regime = 1, which = 2)

plotDiag(msLVS, regime = 1, which = 3)

plotDiag(msLVS, regime = 2, which = 1)

plotDiag(msLVS, regime = 2, which = 2)

plotDiag(msLVS, regime = 2, which = 3)

#
