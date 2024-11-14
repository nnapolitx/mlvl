setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/exercises/data")

library(lavaan)
library(dplyr)
library(psych)
library(MVN)

base<-read.csv("tallerlgcm.csv", header = T, sep = ";")
names(base)
str(base)
describe(base, skew = F)

# Null model
modelo0 <- "
  i =~ 1*deses16 + 1*deses17 + 1*deses18 + 1*deses19
"
fit0<- growth(modelo0, data = base,
              missing="fiml", se="robust", estimator="ml",
              mimic = "mplus")
summary(fit0, fit.measures= T, standardized=T)
# Intercept is significant, 1.802; variance of i is also significant.

# Linear model with intercept in 2016
modelo1 <- "
  i =~ 1*deses16 + 1*deses17 + 1*deses18 + 1*deses19
  s =~ 0*deses16 + 1*deses17 + 2*deses18 + 3*deses19
"
fit1<-growth(modelo1, data = base,
             missing = "fiml", se = "robust", estimator = "mlr")
summary(fit1, fit.measures=T, standardized=T)
# Ajustes del modelo: se rechaza Ho para el chi2, RCFI y RTLI son buenos (0.973
# y 0.967), R RMSEA es bueno, SRMR es bueno.
# Los parametros son significativos (hay cambios en deses en los 4 tiempos), y 
# las varaianzas son significativos, se puede agregar predictor(es).

# modelo cuadratico
modelo2 <- "
  i =~ 1*deses16 + 1*deses17 + 1*deses18 + 1*deses19
  s =~ 0*deses16 + 1*deses17 + 2*deses18 + 3*deses19
  q =~ 0*deses16 + 1*deses17 + 4*deses18 + 9*deses19
"
fit2<-growth(modelo2, data = base,
             missing = "fiml", se = "robust", estimator = "mlr")
summary(fit2, fit.measures=T, standardized=T)
# Se ajusta mejor: acepta Ho, RCFI y RTLI son >= 1.0; RMSEA = 0 y SRMR=0.002
# Intercepto es signif, slope no, y cuad es justo signif (0.045)
# Sin embargo, la varianza de i, s y q no son signif. No se puede agregar algun
# predictor. 
# var of slope is negative may have to correct. fix var to 0 =>
modelo2_1 <- "
  i =~ 1*deses16 + 1*deses17 + 1*deses18 + 1*deses19
  s =~ 0*deses16 + 1*deses17 + 2*deses18 + 3*deses19
  q =~ 0*deses16 + 1*deses17 + 4*deses18 + 9*deses19
  
  s ~~ 0*s
  deses16 ~ 0*deses16
  deses17 ~ 0*deses17
  deses18 ~ 0*deses18
  deses19 ~ 0*deses19
"
fit2_1<-growth(modelo2_1, data = base,
             missing = "fiml", se = "robust", estimator = "mlr")
summary(fit2_1, fit.measures=T, standardized=T)
# sigue negativo o no ident. se queda con modelo lineal por esta razon

# esto ya no es necesario
anova(fit0, fit1, fit2)
# Segun los resultados, el chi2 ajusta mejor para el modelo q. A pesar de esto,
# pareciera que lineal explica mejor el fenom, ya que permite agregar predicts.

# Modelo 3: Estime el modelo seleccionado, incorporando la edad de la persona 
# como predictor de los parámetros de cambio. Interprete los resultados. 
modelo3 <- "
  i =~ 1*deses16 + 1*deses17 + 1*deses18 + 1*deses19
  s =~ 0*deses16 + 1*deses17 + 2*deses18 + 3*deses19
  i ~ edad
  s ~ edad
"
fit3 <- growth(modelo3, data = base, missing="fiml", se="robust", 
               estimator="mlr")
summary(fit3, fit.measures= T, standardized=T)
# Ajusta excelente; edad predice deses en los cuatro momentos. A ver lo que pasa
# cuando edad es libre est... parece que esto no es necesario
modelo3_1 <- "
  i =~ 1*deses16 + 1*deses17 + 1*deses18 + 1*deses19
  s =~ 0*deses16 + 1*deses17 + 2*deses18 + 3*deses19
  
  deses16 ~ edad
  deses17 ~ edad
  deses18 ~ edad
  deses19 ~ edad
"
fit3_1 <- growth(modelo3_1, data = base, missing="fiml", se="robust", 
               estimator="mlr")
summary(fit3_1, fit.measures= T, standardized=T)
# El ajuste se emperora poco, y edad sigue prediciendo/corr con deses.


# Modelos 4: Estime el modelo seleccionado, incorporando los parámetros de 
# cambio como predictores de satisfacción con la vida en 2019. Interprete los 
# resultados.
modelo4 <- "
  i =~ 1*deses16 + 1*deses17 + 1*deses18 + 1*deses19
  s =~ 0*deses16 + 1*deses17 + 2*deses18 + 3*deses19
  
  satis19 ~ 0*i
  satis19 ~ 0*s
"
fit4 <- growth(modelo4, data = base, missing="fiml", se="robust", 
                 estimator="mlr")
summary(fit4, fit.measures= T, standardized=T)


