# Series de Tiempo, Diciembre de 2020
# Clase 24. Datos Panel -- VAR(p)
#****************************************************************************************
#install.packages("panelvar")
#

library(panelvar)

# 
#****************************************************************************************
# Ejemplo 1: Download data

# We used the dynamic panel literature by Arellano and Bond (1991), 
# Blundell and Bond (1998) and Roodman (2009b). 
# This data set describes employment, wages, capital and output of 140 firms in 
# the United Kingdom from 1976 to 1984. 
# We estimate: Employment is explained by past values of employment ("l" lags), 
# current and first lag of wages and output and current value of capital. 

data("abdata")

names(abdata)

# 
#****************************************************************************************
# Estimación

?pvargmm

Arellano_Bond_1991_table4b <- pvargmm(
  dependent_vars = c("n"),
  lags = 2,
  exog_vars = c("w", "wL1", "k", "ys", "ysL1",
                "yr1979", "yr1980", "yr1981", "yr1982",
                "yr1983", "yr1984"),
  transformation = "fd",
  data = abdata,
  panel_identifier = c("id", "year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = FALSE)
summary(Arellano_Bond_1991_table4b)

# 
#****************************************************************************************
# Ejemplo 2: Download data

# We used the panel data set consists of 265 Swedish municipalities and 
# covers 9 years (1979-1987). 
# These variables include total expenditures (expenditures), 
# total own-source revenues (revenues) and intergovernmental grants 
# received by the municipality (grants). 
# Source: Dahlberg and Johansson (2000) 
# Grants from the central to the local government are of three kinds: 
# support to municipalities with small tax capacity, 
# grants toward the running of certain local government activities and 
# grants toward certain investments.

data("Dahlberg")

names(Dahlberg)

# 
#****************************************************************************************
# Estimación

ex1_dahlberg_data <- pvargmm(dependent_vars = c("expenditures", "revenues", "grants"),
          lags = 1,
          transformation = "fod",
          data = Dahlberg,
          panel_identifier=c("id", "year"),
          steps = c("twostep"),
          system_instruments = FALSE,
          max_instr_dependent_vars = 99,
          max_instr_predet_vars = 99,
          min_instr_dependent_vars = 2L,
          min_instr_predet_vars = 1L,
          collapse = FALSE
  )

summary(ex1_dahlberg_data)

# model selection procedure of Andrews and Lu (2001) to select the optimal lag length for our example

Andrews_Lu_MMSC(ex1_dahlberg_data)

#
ex2_dahlberg_data <- pvargmm(dependent_vars = c("expenditures", "revenues", "grants"),
                             lags = 2,
                             transformation = "fod",
                             data = Dahlberg,
                             panel_identifier=c("id", "year"),
                             steps = c("twostep"),
                             system_instruments = FALSE,
                             max_instr_dependent_vars = 99,
                             max_instr_predet_vars = 99,
                             min_instr_dependent_vars = 2L,
                             min_instr_predet_vars = 1L,
                             collapse = FALSE)

Andrews_Lu_MMSC(ex2_dahlberg_data)

# stability of the autoregressive process:
stab_ex1_dahlberg_data <- stability(ex1_dahlberg_data)

print(stab_ex1_dahlberg_data)

plot(stab_ex1_dahlberg_data)

# generate impulse response functions.
ex1_dahlberg_data_oirf <-  oirf(ex1_dahlberg_data, n.ahead = 8)

ex1_dahlberg_data_girf <-  girf(ex1_dahlberg_data, n.ahead = 8, ma_approx_steps= 8)

ex1_dahlberg_data_bs <-  bootstrap_irf(ex1_dahlberg_data, typeof_irf = c("GIRF"),
                                       n.ahead = 8,
                                       nof_Nstar_draws = 500,
                                       confidence.band = 0.95)

plot(ex1_dahlberg_data_girf, ex1_dahlberg_data_bs)

# 
