# Series de Tiempo, Diciembre de 2020
# Clase 22. ARDL
#****************************************************************************************
#install.packages("ARDL")

library(zoo) 
library(xts) # 
library(ARDL)

#****************************************************************************************
# DESCRIPCIÓN DEL PROBLEMA
# Supongamos que queremos modelar el logaritmo de dinero (M2) como una función 
# de LRY (logarithm of real income), IBO (bond rate) e IDE (bank deposit rate). 
# El problema es que la aplicación de una regresión de MCO en datos no estacionarios daría lugar 
# a una regresión espúria. 
# Los parámetros estimados serían consistentes solo si las series estuvieran cointegradas.
# 

#****************************************************************************************
# DATOS:
data(denmark)
#?denmark # see for more information
names(denmark)

#****************************************************************************************
# Procedimiento:
# 1. Calculamos un auto ADRL para determinar el óptimo:

models <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark, max_order = 5)

names(models)

models$top_orders

models$best_order

models$best_model

BestMod <- models$best_model

summary(BestMod)

# 2. UECM (Unrestricted Error Correction Model) of the underlying ARDL

UECM_BestMod <- uecm(BestMod)

summary(UECM_BestMod)

# 3. RECM (Restricted Error Correction Model) of the underlying ARDL
#  allowing the constant to join the short-run relationship (case 2), instead of the long-run (case 3)

RECM_BestMod <- recm(UECM_BestMod, case = 2)

summary(RECM_BestMod)

# 4. long-run levels relationship (cointegration) 

bounds_f_test(BestMod, case = 2)

# 5. Long-run multipliers (with standard errors, t-statistics and p-values)

multipliers(BestMod)

Result <- coint_eq(BestMod, case = 2)

#****************************************************************************************
# Make the plot

Datos <- cbind.zoo(LRM = denmark[,"LRM"], Result)

Datos <- xts(Datos)

plot(Datos, legend.loc = "right")

