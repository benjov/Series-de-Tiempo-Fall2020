# Series de Tiempo, Diciembre de 2020
# Clase 14. ARCH y GARCH Multivariados
#****************************************************************************************
#install.packages("quantmod")
#install.packages("rmgarch")
# install.packages("MTS")

library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(parallel)
library(rugarch)
library(rmgarch)  
library(MTS)

# 
#****************************************************************************************

# Download data from YahooFinance

# Example:
#
getSymbols("TSLA", from = "2020-11-01", 
           to = "2020-11-30", 
           auto.assign = FALSE)

#
Ad(getSymbols("TSLA", from = "2020-11-01", 
              to = "2020-11-30", 
              auto.assign = FALSE))

# Select a stock set 
stock_namelist <- c("AAPL", "PFE", "TSLA")


prices <- xts()

for (stock_index in 1:length(stock_namelist)){
  prices <- cbind(prices, Ad(getSymbols(stock_namelist[stock_index], 
                                        from = "2011-01-01", 
                                        to = "2020-11-30", 
                                        auto.assign = FALSE)))
}
  
#
colnames(prices) <- stock_namelist

#
indexClass(prices) <- "Date"

#
head(prices)

#
logreturns <- diff(log(prices))[-1]

#
head(logreturns)

#****************************************************************************************
# Plot the three series of levells, log-prices and difference
#
plot(prices, 
     col = c("darkred", "darkblue", "darkgreen"),
     main = "Prices of the three select stocks", 
     legend.loc = "topleft" )

#
plot(log(prices), 
     col = c("darkred", "darkblue", "darkgreen"),
     main = "Log-prices of the three select stocks", 
     legend.loc = "topleft" )

#
plot(logreturns, 
     col = c("darkred", "darkblue", "darkgreen"),
     main = "Log-returns of the three select stocks", 
     legend.loc = "topleft" )

#
par(mfrow=c(3, 1))

plot(logreturns$AAPL, 
     col = "darkred",
     main = "Log-returns", 
     legend.loc = "topleft" )

plot(logreturns$PFE, 
     col = "darkblue",
     main = "Log-returns", 
     legend.loc = "topleft" )

plot(logreturns$TSLA, 
     col = "darkgreen",
     main = "Log-returns", 
     legend.loc = "topleft" )

par(mfrow=c(1,1))

#****************************************************************************************

# Determinamos Efectos ARCH
MarchTest(logreturns, lag = 3)

#****************************************************************************************
# GARCH univarite

ugarch_spec <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                          variance.model = list(model = "eGARCH", garchOrder = c(1, 1)))

# Multivariate model
dcc_spec <- dccspec(uspec = multispec(replicate(ugarch_spec, n = 3)),
                    VAR = TRUE, lag = 3,
                    model = "DCC",
                    dccOrder = c(1, 1))

# Estimate model
garchdcc_fit <- dccfit(dcc_spec, 
                       data = logreturns, 
                       solver = "nlminb")
garchdcc_fit

#****************************************************************************************
#
#plot(garchdcc_fit)


#
