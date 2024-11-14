setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/exercises/data")

library(lavaan)
library(dplyr)
library(MVN)
library(psych)

base<-read.csv("taller2.csv", header =T, sep = ";")

str(base)
names(base)
describe(base)
# No hay datos perdidos, el promedio de consumo en los cuatro años va aumentando,
# al parecer, cuadraticamente porque el cambio duplica. El SD en consumo4 es mucho
# más alto comparado con los años anteriores.

table(base$sexo)
# Hay 323 mujeres y 324 hombres

mvn(base[3:6],univariateTest = "SW", desc =F)
# No haay normalidad multivariada, indica que la var no se distr normalmente en
# la pop. Hay que usar 'MLR'.

mvn(base[3:6], mvnTest = "mardia", multivariatePlot = "qq",
    multivariateOutlierMethod = "quan", desc =F)

# Modelo de intercepto aleatorio
modeloi <-"
  i =~ 1*consumo1 + 1*consumo2 + 1*consumo3 + 1*consumo4
"
fiti <-growth(modeloi,data=base, missing="fiml", se="robust",estimator="mlr")
summary(fiti, fit.measures = TRUE, standardized=TRUE)
# El fit es muy malo, Chi2(df) = 1557.095(8), p<.000. Revisar modelo lin

modelol <-"
  i=~ 1*consumo1 + 1*consumo2 + 1*consumo3 + 1*consumo4
  s=~ 0*consumo1 + 1*consumo2 + 2*consumo3 + 3*consumo4
"
fitl <-growth(modelol,data=base, missing="fiml", se="robust",estimator="mlr")
summary(fitl, fit.measures = TRUE, standardized=TRUE)
# Mal ajuste, pasar al cuadratico

modeloq <- "
  i=~ 1*consumo1 + 1*consumo2 + 1*consumo3 + 1*consumo4
  s=~ 1*consumo1 + 2*consumo2 + 3*consumo3 + 4*consumo4
  q=~ 0*consumo1 + 1*consumo2 + 4*consumo3 + 9*consumo4
"
fitq <-growth(modeloq,data=base, missing="fiml", se="robust",estimator="mlr")
summary(fitq, fit.measures = TRUE, standardized=TRUE)

# Estimacion del modelo que incorpora el monitoreo parental 
modelop <- "
  i=~ 1*consumo1 + 1*consumo2 + 1*consumo3 + 1*consumo4
  s=~ 1*consumo1 + 2*consumo2 + 3*consumo3 + 4*consumo4
  q=~ 0*consumo1 + 1*consumo2 + 4*consumo3 + 9*consumo4
  
  i ~ monitoreo
  s ~ monitoreo
  q ~ monitoreo
"
fitp <-growth(modelop,data=base, missing="fiml", se="robust",estimator="mlr")
summary(fitp, fit.measures = TRUE, standardized=TRUE)

