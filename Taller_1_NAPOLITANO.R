setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/exercises/data")

library(lavaan)
library(psych)
library(MVN)
library(dplyr)

base<- read.csv("ansiedad.csv", header = T, sep = ";")
names(base)
describe(base)
# No hay datos perdidos.

mvn(base)
# no hay normalidad multivariada, tampoco univariada en ninguna variable. Hay
# que estimar con MLR.

# modelo medicion
mod0 <- '
AnsT1 =~ item1_t1 + item2_t1 + item3_t1 + item4_t1 + item5_t1
ansT2 =~ item1_t2 + item2_t2 + item3_t2 + item4_t2 + item5_t2
'
fit0 <- cfa(mod0, data = base, estimator = "MLR", std.lv= T, 
            meanstructure = TRUE)
summary(fit0, fit.measures =T, standardized = T, rsquare = T)

# Results: Chi-square no es signif (0.512) y todos los otros indicadores de
# ajuste muestran un muy buen ajuste (CFI=1, TLI=1, Rob RMSEA= 0.00 [0.00-0.04]
# SRMR = 0.029). Todas las cargas factoriales son significativas y altas (.58 o
# mayor). No hay covarianza significativa entre T1 y T2. Finalmente, hay bastante
# varianza de las VL exmplicada por los factores (minimo 0.346).

# Evaluacion de modelo para invarianza entre las mediciones (T1 y T2). Si hay 
# invarianza, significaria que las mediciones son factorialmente invariantes. 
# Esto podria significar que las mediciones de ansiedad no varian en el tiempo;
# que es un rasgo mas estable en el tiempo???.

configural <- '
AnsT1 =~ item1_t1 + item2_t1 + item3_t1 + item4_t1 + item5_t1
ansT2 =~ item1_t2 + item2_t2 + item3_t2 + item4_t2 + item5_t2

# autocor en el tiempo
item1_t1 ~~ item1_t2
item2_t1 ~~ item2_t2
item3_t1 ~~ item3_t2
item4_t1 ~~ item4_t2
item5_t1 ~~ item5_t2
'

fit_config <- sem(configural, estimator="MLR", std.lv=F, sample.mean=T, data=base)
summary(fit_config, fit.measures=T, standardized=T, rsquare=T)
# Results: Chi2 no es signif (0.409), y los indicadores de ajuste son buenos (
# CFI = 0.999, TLI=0.998, Rob RMSEA=0.011 [0.00-0.046], SRMR=0.28)
# Todas las cargas fact son significativas y mayores a 0.588.
# La covarianza entre los itemes en el tiempo no es sig.
# interceptos son significativas
# La varianza si es significativa para todas las items.

debil <- '
AnsT1 =~ l1*item1_t1 + l2*item2_t1 + l3*item3_t1 + l4*item4_t1 + l5*item5_t1
ansT2 =~ l1*item1_t2 + l2*item2_t2 + l3*item3_t2 + l4*item4_t2 + l5*item5_t2

# Interceptos libre estimados
item1_t1 ~ 1
item2_t1 ~ 1
item3_t1 ~ 1
item4_t1 ~ 1
item5_t1 ~ 1
item1_t2 ~ 1
item2_t2 ~ 1
item3_t2 ~ 1
item4_t2 ~ 1
item5_t2 ~ 1

# Residuos libre estimados
item1_t1 ~~ item1_t1
item2_t1 ~~ item2_t1
item3_t1 ~~ item3_t1
item4_t1 ~~ item4_t1
item5_t1 ~~ item5_t1
item1_t2 ~~ item1_t2
item2_t2 ~~ item2_t2
item3_t2 ~~ item3_t2
item4_t2 ~~ item4_t2
item5_t2 ~~ item5_t2

# Varianza de los factores libre
AnsT1 ~~ 1*AnsT1
ansT2 ~~ 1*ansT2

# Medias de los factores en 0
AnsT1 ~ 0*1
ansT2 ~ 0*1

# Residuos de los item en el tiempo corr
item1_t1 ~~ item1_t2
item2_t1 ~~ item2_t2
item3_t1 ~~ item3_t2
item4_t1 ~~ item4_t2
item5_t1 ~~ item5_t2

# covar entre los factores
AnsT1 ~~ ansT2
'

fit_debil <- sem(debil, estimator = 'MLR', sample.mean = T, data = base)
summary(fit_debil, fit.measures=T, standardized=T)
# Results: Chi2 now significant (0.000), but R.CFI=0.963, R.TLI=0.952, R.RMSEA=
# 0.063 [0.044-0.082], SRMR indica un mal ajuste (0.152).
# Las cargas fact son todas signif con un min de 0.645.
# Ninguna covar es signif
# la var es signif en todas item y tambien los interceptos.

fuerte <- '
AnsT1 =~ l1*item1_t1 + l2*item2_t1 + l3*item3_t1 + l4*item4_t1 + l5*item5_t1
ansT2 =~ l1*item1_t2 + l2*item2_t2 + l3*item3_t2 + l4*item4_t2 + l5*item5_t2

# Interceptos fijos igual en tiempo
item1_t1 ~ i1*1
item2_t1 ~ i2*1
item3_t1 ~ i3*1
item4_t1 ~ i4*1
item5_t1 ~ i5*1
item1_t2 ~ i1*1
item2_t2 ~ i2*1
item3_t2 ~ i3*1
item4_t2 ~ i4*1
item5_t2 ~ i5*1

# Residuos libre estimados
item1_t1 ~~ item1_t1
item2_t1 ~~ item2_t1
item3_t1 ~~ item3_t1
item4_t1 ~~ item4_t1
item5_t1 ~~ item5_t1
item1_t2 ~~ item1_t2
item2_t2 ~~ item2_t2
item3_t2 ~~ item3_t2
item4_t2 ~~ item4_t2
item5_t2 ~~ item5_t2

# Varianza de los factores libre
AnsT1 ~~ 1*AnsT1
ansT2 ~~ ansT2

# Medias de los factores libres
AnsT1 ~ 0*1
ansT2 ~ 1

# Residuos de los item en el tiempo corr
item1_t1 ~~ item1_t2
item2_t1 ~~ item2_t2
item3_t1 ~~ item3_t2
item4_t1 ~~ item4_t2
item5_t1 ~~ item5_t2

# covar entre los factores
AnsT1 ~~ ansT2
'

fit_fuerte <- sem(fuerte, estimator = 'MLR', sample.mean = T, data = base)
summary(fit_fuerte, fit.measures=T, standardized=T)
# Results: Chi2 es signif (0.000)



