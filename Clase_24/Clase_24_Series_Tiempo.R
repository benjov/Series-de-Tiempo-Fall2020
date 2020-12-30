# Series de Tiempo, Diciembre de 2020
# Clase 23. Datos Panel -- Raíces unitarias para panel
#****************************************************************************************
#install.packages("plm")
#

library(plm)
library(ggplot2)
library(reshape2)

# 
#****************************************************************************************
# Download data from library PLM

data("EmplUK", package="plm")

data("Produc", package="plm")

data("Grunfeld", package="plm")

data("Wages", package="plm")

# 
#****************************************************************************************
# Describe data

names(Grunfeld)
#  Grunfeld data (Grunfeld 1958) comprising 20 annual observations on
#  the three variables real gross investment (invest), real value of 
# the firm (value), and real value of the capital stock (capital) 
# for 10 large US firms for the years 1935–1954

head(Grunfeld)

Invest <- data.frame(split( Grunfeld$inv, Grunfeld$firm )) # individuals in columns

names(Invest)

names(Invest) <- c("Firm_1", "Firm_2", "Firm_3", "Firm_4", "Firm_5", "Firm_6", "Firm_7",
                   "Firm_8", "Firm_9", "Firm_10")

names(Invest)

# 
#****************************************************************************************
# Plot:

plot(Invest$Firm_1, type = "l", col = 1, ylim = c(0, 1500), lty = 1,
     xlab = "Tiempo", ylab = "Real gross investment")
lines(Invest$Firm_2, type = "l", col = 2, lty = 2)
lines(Invest$Firm_3, type = "l", col = 3, lty = 1)
lines(Invest$Firm_4, type = "l", col = 4, lty = 2)
lines(Invest$Firm_5, type = "l", col = 5, lty = 1)
lines(Invest$Firm_6, type = "l", col = 6, lty = 2)
lines(Invest$Firm_7, type = "l", col = 7, lty = 1)
lines(Invest$Firm_8, type = "l", col = 8, lty = 2)
lines(Invest$Firm_9, type = "l", col = 9, lty = 1)
lines(Invest$Firm_10, type = "l", col = 10, lty = 2)
legend("topleft", legend=c("Firm_1", "Firm_2", "Firm_3", "Firm_4", "Firm_5",
                           "Firm_6", "Firm_7", "Firm_8", "Firm_9", "Firm_10"),
       col = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), lty = 1:2)

# 
#****************************************************************************************
# Unit Root Test:

?purtest
# test specifies the type of test to be performed among 
# Levin et al. (2002), Im et al. (2003), Maddala and Wu (1999) and Hadri (2000)

# Consider Levin et al. (2002)
purtest(log(Invest), test = "levinlin", exo = "intercept", 
        lags = "AIC", pmax = 4)

## same via:

ts_LInvest <- ts(log(Invest), start = 1935, end = 1954, freq = 1)

ts_DLInvest <- diff(ts(log(Invest), start = 1935, end = 1954, freq = 1), 
                    lag = 1, differences = 1)

# 
purtest(ts_LInvest, test = "levinlin", exo = "intercept", 
        lags = "AIC", pmax = 4)

summary(purtest(ts_LInvest, test = "levinlin", exo = "intercept", 
                lags = "AIC", pmax = 4))

#
purtest(ts_DLInvest, test = "levinlin", exo = "intercept", 
        lags = "AIC", pmax = 4)

summary(purtest(ts_DLInvest, test = "levinlin", exo = "intercept", 
                lags = "AIC", pmax = 4))

# Consider Im-Pesaran-Shin Unit-Root Test (2003)

purtest(ts_LInvest, test = "ips", exo = "intercept", 
        lags = "AIC", pmax = 4)

summary(purtest(ts_LInvest, test = "ips", exo = "intercept", 
                lags = "AIC", pmax = 4))

#
purtest(ts_DLInvest, test = "ips", exo = "intercept", 
        lags = "AIC", pmax = 4)

summary(purtest(ts_DLInvest, test = "ips", exo = "intercept", 
                lags = "AIC", pmax = 4))

