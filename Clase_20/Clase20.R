# Series de Tiempo, Diciembre de 2020
# Clase 20. Aplicacion GARCH(p,q)
#****************************************************************************************
#install.packages("ugarch")
#install.packages("tseries")
library(parallel)
library(xts)
library(zoo)
library(Matrix)
library(rugarch)
library(quantmod)
library(tseries)
library(expm)
library(Matrix)
library(ggplot2)
library(moments)
library(dynlm)
library(broom)
library(FinTS)
library(lubridate)
library(forecast)
library(readxl)
library(MASS)
library(tsbox)

## Obtener datos de las acciones Amazon mediante la libreria 'quantmod'

options("getSymbols.warning4.0"=FALSE)

AMZN <-getSymbols("AMZN", src = "yahoo", auto.assign = FALSE)

AMZN <- na.omit(AMZN)

plot(AMZN$AMZN.Adjusted)

logret <- ts(diff(log(AMZN$AMZN.Adjusted))[-1])

plot(logret)

logret_mean = dynlm(logret~1)

summary(logret_mean)

ehatsq = ts(resid(logret_mean)^2)




##estimación ARCH

model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1,0)), 
                        mean.model = list(armaOrder = c(0,0)), distribution.model = "std")

fit.arch.n = ugarchfit(spec = model.spec, data = logret, solver = "solnp")
fit.arch.n@fit$matcoef

boot.garch <- ugarchboot(fit.arch.n,
                         method = "Partial",
                         sampling = "raw",  #bootstrap from fitted varepsilon
                         n.ahead = 1,          #simulation horizon
                         n.bootpred = 100000, #number of simulations 
                         solver = "solnp")
## VaR estimado mediante ARCH

rvec <- boot.garch@fseries
alpha <- 0.05
VaR <- quantile(rvec, alpha)
VaR

F_inv = qt(0.05, 2.9079)

tiempo= time(AMZN[-1])

ggplot()+
  geom_point(aes(y = logret,x = tiempo),colour = 'lightgrey' , size = 2) + 
  geom_line(aes(y = fit.arch.n@fit$sigma*(F_inv), x = tiempo), colour = 'red') +
  geom_hline(yintercept = sd(logret)*qnorm(0.05) , colour = 'darkgreen' , size = 1.2) + theme_light() + 
  labs(x = '' , y = 'Daily Returns' , title = 'Value at Risk Comparison')

##estimación GARCH

model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(0,1)), 
                        mean.model = list(armaOrder = c(0,0)), distribution.model = "std")

fit.garch.n = ugarchfit(spec = model.spec, data = logret, solver = "solnp")
fit.garch.n@fit$matcoef

boot.garch <- ugarchboot(fit.garch.n,
                         method = "Partial",
                         sampling = "raw",  #bootstrap from fitted varepsilon
                         n.ahead = 1,          #simulation horizon
                         n.bootpred = 100000, #number of simulations 
                         solver = "solnp")
## VaR estimado mediante GARCH
rvec <- boot.garch@fseries
alpha <- 0.05
VaR <- quantile(rvec, alpha)
VaR

F_inv = qt(0.05, 2.9079)

tiempo= time(AMZN[-1])

ggplot()+
  geom_point(aes(y = logret,x = tiempo),colour = 'lightgrey' , size = 2) + 
  geom_line(aes(y = fit.garch.n@fit$sigma*(F_inv), x = tiempo), colour = 'red') +
  geom_hline(yintercept = sd(logret)*qnorm(0.05) , colour = 'darkgreen' , size = 1.2) + theme_light() + 
  labs(x = '' , y = 'Daily Returns' , title = 'Value at Risk Comparison')


#forma alternativa de estimar GARCH

summary(garch(logret, order = c(0,1)))


#Criterios de información 

infocriteria(fit.garch.n)


#Selección del modelo óptimo

source("Lag_Opt_GARCH.R")

Lag_Opt_GARCH(ehatsq,4,4)


# Estimación de modelo óptimo 

model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1,1)), 
                        mean.model = list(armaOrder = c(0,0)), distribution.model = "std")


model.fit = ugarchfit(spec = model.spec , data = logret, solver = 'solnp')

model.fit@fit$matcoef


boot.garch <- ugarchboot(model.fit,
                         method = "Partial",
                         sampling = "raw",  #bootstrap from fitted varepsilon
                         n.ahead = 1,          #simulation horizon
                         n.bootpred = 100000, #number of simulations 
                         solver = "solnp")

## VaR estimado mediante GARCH óptimo
rvec <- boot.garch@fseries
alpha <- 0.05
VaR <- quantile(rvec, alpha)
VaR

F_inv = qt(0.05, 2.9079)

tiempo= time(AMZN[-1])

ggplot()+
  geom_point(aes(y = logret,x = tiempo),colour = 'lightgrey' , size = 2) + 
  geom_line(aes(y = model.fit@fit$sigma*(F_inv), x = tiempo), colour = 'red') +
  geom_hline(yintercept = sd(logret)*qnorm(0.05) , colour = 'darkgreen' , size = 1.2) + theme_light() + 
  labs(x = '' , y = 'Daily Returns' , title = 'Value at Risk Comparison')


## comparativo entre cálculos 
ggplot()+
  geom_point(aes(y = logret,x = tiempo),colour = 'lightgrey' , size = 2) + 
  geom_line(aes(y = model.fit@fit$sigma*(F_inv), x = tiempo), colour = 'red4') +
  geom_line(aes(y = fit.arch.n@fit$sigma*(F_inv), x = tiempo), colour = 'red', alpha = 0.25) +
  geom_line(aes(y = fit.garch.n@fit$sigma*(F_inv), x = tiempo), colour = 'red2') +
  geom_hline(yintercept = sd(logret)*qnorm(0.05) , colour = 'darkgreen') + theme_light() + 
  labs(x = '' , y = 'Daily Returns' , title = 'Value at Risk Comparison')
