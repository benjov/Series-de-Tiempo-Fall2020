# Series de Tiempo, Enero de 2020
# Clase 26. Markov Regime Switching Model (MSM)
#****************************************************************************************
#install.packages("MSwM") #if not yet installed

library(parallel)
library(MSwM)

# Example:
#****************************************************************************************

# Data: Number of deaths in traffic accidents in Spain during the year 2010, the average 
# daily temperature and the daily sum of precipitations. 
# We study the relation between the number of deaths with the climate conditions. 
# We illustrate the use of a Markov Switching Model in this case because there exists a 
# different behaviour between the variables during weekends and working days. 

#
#****************************************************************************************
# Data:

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
