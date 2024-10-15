library(lavaan)
library(MVN)
library(psych)
library(dplyr)
setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/clases/data")

base<- read.csv("ejercicio1.csv", header = T, sep = ";", na=-9)
names(base)

str(base)
# Output muestra que los datos son char y tienen que ser numeros.

base[3:17] <- lapply(base[3:17], as.numeric)
str(base)
# arreglado

describe(base[3:17], skew =F)

mvn(base[3:17])
# no hay normalidad

# especificacion del modelo
mod1 <- '
  perf1 =~ psp1.T1 + psp2.T1 + psp3.T1
  perf1 ~~ 1*perf1
'

fit1 <- cfa(mod1, std.lv=T, estimator = "MLR", meanstructure = T, data = base)
summary(fit1, fit.measures = T, standardized = T, rsquare = T)

# pregunta sobre los indicadores de ajuste; esto es resultado de tener un 
# modelo identificado, o "just-identified model"? No se si hice esta parte bien

configural <- '
  perf1 =~ psp1.T1 + psp2.T1 + psp3.T1
  perf3 =~ psp1.T3 + psp2.T3 + psp3.T3
  perf5 =~ psp1.T5 + psp2.T5 + psp3.T5
  
# interceptos libremente estimados
  psp1.T1 ~ 1
  psp2.T1 ~ 1
  psp3.T1 ~ 1
  psp1.T3 ~ 1
  psp2.T3 ~ 1
  psp3.T3 ~ 1
  psp1.T5 ~ 1
  psp2.T5 ~ 1
  psp3.T5 ~ 1
 
# Residuos libremente estimados
  psp1.T1 ~~ psp1.T1
  psp2.T1 ~~ psp2.T1
  psp3.T1 ~~ psp3.T1
  psp1.T3 ~~ psp1.T3
  psp2.T3 ~~ psp2.T3
  psp3.T3 ~~ psp3.T3
  psp1.T5 ~~ psp1.T5
  psp2.T5 ~~ psp2.T5
  psp3.T5 ~~ psp3.T5
 
# Varianza de los factores fijos en 1 (identificación)
  perf1 ~~ 1*perf1
  perf3 ~~ 1*perf3
  perf5 ~~ 1*perf5
 
# Medias de los factores fijos en 0 (identificación)
  perf1 ~ 0*1
  perf3 ~ 0*1
  perf5 ~ 0*1
  
# Residuos de los ítems en el tiempo correlacionados
  psp1.T1 ~~ psp1.T3
  psp2.T1 ~~ psp2.T3
  psp3.T1 ~~ psp3.T3
  psp1.T3 ~~ psp1.T5
  psp2.T3 ~~ psp2.T5
  psp3.T3 ~~ psp3.T5
  psp1.T1 ~~ psp1.T5
  psp2.T1 ~~ psp2.T5
  psp3.T1 ~~ psp3.T5
 
# Covarianza entre los factores
  perf1 ~~ perf3
  perf3 ~~ perf5
  perf1 ~~ perf5
'
fit_configural <- cfa(configural, data = base, std.lv=T,
                      estimator = "MLR", mimic = "mplus")

summary(fit_configural, fit.measures = TRUE, standardized = T)

# resultados del modelo configural: el chi2 acepta la Ho, y CFI y TLI indican 
# buen ajuste. RMSEA tiene buen ajuste, y el intervalo de confianza superior
# supera levamente a 0,10 (0,103). Adicionalmente el SRMR < 0,08. Hay invar
# configural (debil). Finalmente, la carga fact en los 3 momento no es menor de
# 0.88; indicando un buen ajuste al modelo.

metric <- '
  perf1 =~ l1*psp1.T1 + l2*psp2.T1 + l3*psp3.T1
  perf3 =~ l1*psp1.T3 + l2*psp2.T3 + l3*psp3.T3
  perf5 =~ l1*psp1.T5 + l2*psp2.T5 + l3*psp3.T5
  
# interceptos libremente estimados
  psp1.T1 ~ 1
  psp2.T1 ~ 1
  psp3.T1 ~ 1
  psp1.T3 ~ 1
  psp2.T3 ~ 1
  psp3.T3 ~ 1
  psp1.T5 ~ 1
  psp2.T5 ~ 1
  psp3.T5 ~ 1
 
# Residuos libremente estimados
  psp1.T1 ~~ psp1.T1
  psp2.T1 ~~ psp2.T1
  psp3.T1 ~~ psp3.T1
  psp1.T3 ~~ psp1.T3
  psp2.T3 ~~ psp2.T3
  psp3.T3 ~~ psp3.T3
  psp1.T5 ~~ psp1.T5
  psp2.T5 ~~ psp2.T5
  psp3.T5 ~~ psp3.T5
 
# Varianza de los factores fijos en 1 (identificación)
  perf1 ~~ 1*perf1
  perf3 ~~ 1*perf3
  perf5 ~~ 1*perf5
 
# Medias de los factores fijos en 0 (identificación)
  perf1 ~ 0*1
  perf3 ~ 0*1
  perf5 ~ 0*1
  
# Residuos de los ítems en el tiempo correlacionados
  psp1.T1 ~~ psp1.T3
  psp2.T1 ~~ psp2.T3
  psp3.T1 ~~ psp3.T3
  psp1.T3 ~~ psp1.T5
  psp2.T3 ~~ psp2.T5
  psp3.T3 ~~ psp3.T5
  psp1.T1 ~~ psp1.T5
  psp2.T1 ~~ psp2.T5
  psp3.T1 ~~ psp3.T5
 
# Covarianza entre los factores
  perf1 ~~ perf3
  perf3 ~~ perf5
  perf1 ~~ perf5
'

fit_metric <- cfa(metric, data = base, std.lv=T,estimator = "MLR", 
                  mimic = "mplus")
summary(fit_metric, fit.measures = TRUE, standardized = T)

# resultados del modelo metric (debil): el chi2 acepta la Ho, y CFI y TLI indican 
# buen ajuste. RMSEA tiene buen ajuste, y el intervalo de confianza superior
# no supera a 0,10 (0,05 y 0,08 para robusto). Adicionalmente el SRMR < 0,08. 
# Hay invar metric. Finalmente, la carga fact en los 3 momento no es menor de
# 0.87; indicando un buen ajuste al modelo.

scalar <- '
  perf1 =~ l1*psp1.T1 + l2*psp2.T1 + l3*psp3.T1
  perf3 =~ l1*psp1.T3 + l2*psp2.T3 + l3*psp3.T3
  perf5 =~ l1*psp1.T5 + l2*psp2.T5 + l3*psp3.T5
  
# interceptos libremente estimados
  psp1.T1 ~ i1*1
  psp2.T1 ~ i2*1
  psp3.T1 ~ i3*1
  psp1.T3 ~ i1*1
  psp2.T3 ~ i2*1
  psp3.T3 ~ i3*1
  psp1.T5 ~ i1*1
  psp2.T5 ~ i2*1
  psp3.T5 ~ i3*1
 
# Residuos libremente estimados
  psp1.T1 ~~ psp1.T1
  psp2.T1 ~~ psp2.T1
  psp3.T1 ~~ psp3.T1
  psp1.T3 ~~ psp1.T3
  psp2.T3 ~~ psp2.T3
  psp3.T3 ~~ psp3.T3
  psp1.T5 ~~ psp1.T5
  psp2.T5 ~~ psp2.T5
  psp3.T5 ~~ psp3.T5
 
# Varianza de los factores fijos en 1 (identificación)
  perf1 ~~ 1*perf1
  perf3 ~~ 1*perf3
  perf5 ~~ 1*perf5
 
# Medias de los factores fijos en 0 (identificación)
  perf1 ~ 0*1
  perf3 ~ 0*1
  perf5 ~ 0*1
  
# Residuos de los ítems en el tiempo correlacionados
  psp1.T1 ~~ psp1.T3
  psp2.T1 ~~ psp2.T3
  psp3.T1 ~~ psp3.T3
  psp1.T3 ~~ psp1.T5
  psp2.T3 ~~ psp2.T5
  psp3.T3 ~~ psp3.T5
  psp1.T1 ~~ psp1.T5
  psp2.T1 ~~ psp2.T5
  psp3.T1 ~~ psp3.T5
 
# Covarianza entre los factores
  perf1 ~~ perf3
  perf3 ~~ perf5
  perf1 ~~ perf5
'

fit_scalar <- cfa(scalar, data = base, std.lv=T,estimator = "MLR", 
                  mimic = "mplus")
summary(fit_scalar, fit.measures = TRUE, standardized = T)

# resultados del modelo metric (debil): el chi2 acepta la Ho, marginalmente. 
# Sin embargo, todos los otros indicadores de ajuste estan aceptables. El modelo
# tiene scalar (fuerte) invarianza.

anova(fit_configural, fit_metric, fit_scalar)
# Parece que el ajuste metrica tiene el mejor ajuste.

base$sexo <- as.factor(base$sexo)
str(base$sexo)

config2 <- sem(mod1, data = base, estimator = "MLR", group = "sexo")
summary(config2, fit.measures = T, standardized = T, rsquare = T)

# resultados del modelo configural, agrupando hombres y mujeres no cuenta con 
# buen indicadores de ajuste: se rechaza Ho para Chi2; CFI y TLI no alcanzan a
# 0.95; El RMSEA esta a 0.517 y el SRMR esta en 0.258. El modelo no es invariante
# entre mujeres y hombres.

