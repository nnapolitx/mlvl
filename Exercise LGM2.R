setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/exercises/data")

library(lavaan)
library(dplyr)
library(psych)
library(MVN)

base<-read.csv("Taller_clase6.csv", header = T, sep = ";")
names(base)
str(base)
describe(base, skew = F)

modelo0<- "
 i=~ 1*monit12 + 1*monit13+ 1*monit14 + 1*monit15 +
 1*monit16 + 1*monit17+ 1*monit18
 "

fit0<- growth(modelo0, data = base,
              missing="fiml", se="robust", estimator="mlr",
              mimic = "mplus")

summary(fit0, fit.measures= T, standardized=T)

# UNFINISHED, SEE PDF LGCM



