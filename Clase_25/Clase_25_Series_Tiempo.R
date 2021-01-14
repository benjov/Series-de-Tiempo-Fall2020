# Series de Tiempo, Diciembre de 2020
# Clase 25. Modelos de Cambio de Regímen (TAR)
#****************************************************************************************
#install.packages('astsa')
#install.packages("tsDyn") #if not yet installed

library(astsa)
library(tsDyn)

# 
#****************************************************************************************
# Data:
# Monthly rates of deaths due to flu in the United States for 11 years

setwd("/Users/benjaminolivavazquez/Documents/Personal/Cursos_UNAM/SERIES_2021-I/Series-de-Tiempo-Fall2020/Clase_25")

flu <- read.delim("flu.txt")

flu <- ts(flu)

D_flu = diff(flu, lag = 1)

# 
#****************************************************************************************
# Plotting
plot(flu, type = "b", col = "darkred", ylab = "", 
     main = "Monthly rates of deaths due to flu in the United States")

plot(D_flu, type="b", col = "darkred", ylab = "", 
     main = "Diff Monthly rates of deaths due to flu in the United States")

# 
#****************************************************************************************
# Primer aproximación:

table = ts.intersect(D_flu, 
                     lag1 = lag(D_flu, -1), 
                     lag2 = lag(D_flu, -2), 
                     lag3 = lag(D_flu, -3), 
                     lag4 = lag(D_flu, -4))


x = table[, 1]
P = table[, 2:5]
c = .05 ## Threshold value

##Regression for values below the threshold

less = (P[,1] < c)

x1 = x[less]

P1 = P[less,]

out1 = lm(x1 ~ P1[,1] + P1[,2] + P1[,3] + P1[,4])

summary(out1)

##Regression for values above the threshold

greater = (P[,1]>=c)

x2 = x[greater]

P2 = P[greater,]

out2 = lm(x2 ~ P2[,1] + P2[,2] + P2[,3] + P2[,4])

summary(out2)

##Residuals

res1 = residuals(out1)
res2 = residuals(out2)

less[less==1] = res1
greater[greater==1] = res2

resid = less + greater
acf2(resid)

##Predicted values

less = (P[,1] < c)

greater = (P[,1] >= c)

fit1 = predict(out1)

fit2 = predict(out2)

less[less==1] = fit1

greater[greater==1] = fit2

fit = less + greater

plot(D_flu, type="b", col = "darkred", ylab = "", 
     main = "Diff Monthly rates of deaths due to flu in the United States")
lines(fit, col = "darkblue", lty="dashed")

# 
#****************************************************************************************
# The tsDyn package in R has simplified this code into a handful of steps:

?setar

D_flu_tar4_05 <- setar(D_flu, m = 4, thDelay = 0, th = 0.05) 

summary(D_flu_tar4_05) 

plot(D_flu_tar4_05)

# If we do not provide a threshold to the th option, 
# setar searches over a grid to choose a threshold ~ 0.038:

D_flu_tar4 <- setar(D_flu, m = 4, thDelay = 0)

summary(D_flu_tar4)

plot(D_flu_tar4)

#

