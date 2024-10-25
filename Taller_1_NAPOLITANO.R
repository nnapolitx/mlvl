# ---- Setear el directorio del trabajo y cargar el base y las librerias----
setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/exercises/data")

library(lavaan)
library(psych)
library(MVN)
library(dplyr)

base<- read.csv("ansiedad.csv", header = T, sep = ";")
names(base)
describe(base)
# No hay datos perdidos.

# ---- identificar el modelo de medicion----
mvn(base [2:11])
# Hay norm multivariada, per no univariada en ninguna variable. Hay
# que estimar con MLR.

# modelo medicion
mod0 <- '
ansT1 =~ item1_t1 + item2_t1 + item3_t1 + item4_t1 + item5_t1
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

# ---- prueba de invarianza en el tiempo ----

# modelo configural
configural <- '
ansT1 =~ item1_t1 + item2_t1 + item3_t1 + item4_t1 + item5_t1
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

# modelo metrico (weak)
debil <- '
ansT1 =~ l1*item1_t1 + l2*item2_t1 + l3*item3_t1 + l4*item4_t1 + l5*item5_t1
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
ansT1 ~~ 1*ansT1
ansT2 ~~ 1*ansT2

# Medias de los factores en 0
ansT1 ~ 0*1
ansT2 ~ 0*1

# Residuos de los item en el tiempo corr
item1_t1 ~~ item1_t2
item2_t1 ~~ item2_t2
item3_t1 ~~ item3_t2
item4_t1 ~~ item4_t2
item5_t1 ~~ item5_t2

# covar entre los factores
ansT1 ~~ ansT2
'

fit_debil <- sem(debil, estimator = 'MLR', sample.mean = T, data = base)
summary(fit_debil, fit.measures=T, standardized=T)
# Results: Chi2 now significant (0.000), but R.CFI=0.963, R.TLI=0.952, R.RMSEA=
# 0.063 [0.044-0.082]. Sin embargo, SRMR indica un mal ajuste (0.152).
# Las cargas fact son todas signif con un min de 0.645.
# Ninguna covar es signif
# la var es signif en todas item y tambien los interceptos.

# modelo escalar (fuerte)
fuerte <- '
ansT1 =~ l1*item1_t1 + l2*item2_t1 + l3*item3_t1 + l4*item4_t1 + l5*item5_t1
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
ansT1 ~~ 1*ansT1
ansT2 ~~ ansT2

# Medias de los factores libres
ansT1 ~ 0*1
ansT2 ~ 1

# Residuos de los item en el tiempo corr
item1_t1 ~~ item1_t2
item2_t1 ~~ item2_t2
item3_t1 ~~ item3_t2
item4_t1 ~~ item4_t2
item5_t1 ~~ item5_t2

# covar entre los factores
ansT1 ~~ ansT2
'

fit_fuerte <- sem(fuerte, estimator = 'MLR', sample.mean = T, data = base)
summary(fit_fuerte, fit.measures=T, standardized=T)
# Results: Chi2 es signif (0.000), y el R.CFI indica un mal ajuste, ademas de un 
# mal ajuste de R.TLI. Tambien, hay un mal ajuste por parte de R.RMSEA y SRMR.
# Por estas razones, no hay invarianza scalar/fuerte. Se podria examinar si hay
# cambios en el ajuste del modelo si se libera un parametro.

# Probar ajustando el modelo
lavTestScore(fit_fuerte)
parameterEstimates(fit_fuerte, standardized = T)
# liberar items 4 de T1 y T2 para tener mejor ajuste de modelo; solamente para
# explorar

fuerte_modificado <- '
ansT1 =~ l1*item1_t1 + l2*item2_t1 + l3*item3_t1 + l4*item4_t1 + l5*item5_t1
ansT2 =~ l1*item1_t2 + l2*item2_t2 + l3*item3_t2 + l4*item4_t2 + l5*item5_t2

# Interceptos fijos igual en tiempo
item1_t1 ~ i1*1
item2_t1 ~ i2*1
item3_t1 ~ i3*1
item5_t1 ~ i5*1
item1_t2 ~ i1*1
item2_t2 ~ i2*1
item3_t2 ~ i3*1
item5_t2 ~ i5*1

# liberar interceptos de item4 T1 y 2
item4_t1 ~ i4_t1*1
item4_t2 ~ i4_t2*1

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
ansT1 ~~ 1*ansT1
ansT2 ~~ ansT2

# Medias de los factores libres
ansT1 ~ 0*1
ansT2 ~ 1

# Residuos de los item en el tiempo corr
item1_t1 ~~ item1_t2
item2_t1 ~~ item2_t2
item3_t1 ~~ item3_t2
item4_t1 ~~ item4_t2
item5_t1 ~~ item5_t2

# covar entre los factores
ansT1 ~~ ansT2
'

fit_fuerte_mod <- sem(fuerte_modificado, estimator = 'MLR', sample.mean = T, 
                      data = base)
summary(fit_fuerte_mod, fit.measures=T, standardized=T)
# Aun hay mal ajuste, probablemente tendria que liberar los interceptos de items
# 3 y 5 de T1 y 2. 

# Proximo paso es probar formalmente la invarianza metrica/debil contra el 
# modelo configural.
anova(fit_config, fit_debil)
# Segun la prueba de diff chi2, el modelo debil es significativamente diferente 
# al modelo configural, lo cual indica que no hay invariance debil entre las dos 
# olas de medicion. Esto quiere decir que, si bien tenemos un patron de factores
# entre los dos tiempos (configural), las cargas son estadisticamente significa-
# tiva (Little chap 5). Segun est resultado, se podria explorar si hay 
# invarianza debil parcial. Est se puede hacer por liberar uno de los factores.

lavTestScore(fit_debil)
# Segun estos resultados, liberar ninguno de los items seria signif. Pero, item
# 2 es marginalmente signif (p=0.06), por lo tanto, vamos a explorar un modelo
# con esto liberado.

debil_mod <- '
ansT1 =~ l1*item1_t1 + item2_t1 + l3*item3_t1 + l4*item4_t1 + l5*item5_t1
ansT2 =~ l1*item1_t2 + item2_t2 + l3*item3_t2 + l4*item4_t2 + l5*item5_t2

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
ansT1 ~~ 1*ansT1
ansT2 ~~ 1*ansT2

# Medias de los factores en 0
ansT1 ~ 0*1
ansT2 ~ 0*1

# Residuos de los item en el tiempo corr
item1_t1 ~~ item1_t2
item2_t1 ~~ item2_t2
item3_t1 ~~ item3_t2
item4_t1 ~~ item4_t2
item5_t1 ~~ item5_t2

# covar entre los factores
ansT1 ~~ ansT2
'

fit_debil_mod <- sem(debil_mod, estimator = 'MLR', sample.mean = T, 
                     data = base)
summary(fit_debil_mod, fit.measures=T, standardized=T)
# Mirando las differencias en el CFI, entre el modelo config y este nuevo modelo
# modificado, hay una diferencia mayor a 0.01, indicando que, probablemente hay
# una diferencia en las cargas factoriales. Esto sugiere que este modelo sigue
# no invariante, pero se puede probar con la prueba anova:

anova(fit_config,fit_debil_mod)
# sigue con differencia signif.

# conclusiones: El modelo no es invariante en el tiempo, y solamente el modelo 
# configural (el patron de los factores) muestra un buen ajuste. Estos cambios 
# en la magnitud de la carga factorial significan que la manera en que las y los  
# participantes interpretaron, o experimentaron, las preguntas de la escala; 
# indicando un posible limite psicometrica del instrumento. Estos cambios   
# podrian debersea factores contextuales que no son estables en el tiempo como,  
# por ejemplo, la aparicion de feriados o momentos/estaciones que producen mayor
# estres en las personas. La diferencias en las cargas factoriales indican un 
# decremento en items uno, cuatro y cinco, ademas de un aumento en items dos y 
# tres. Segun el detalle de los items, dos y tres corresponden a preocupacion y
# poca abilidad de relajarse, mientras los sentimientos de tension, estados  
# nerviosos y las sintomas fisicas de ansiedad disminuyeron.

# ----Invarianza por grupo (sexo)----

# Para pregunta numero tres: 3. Evalúe si el modelo es invariante entre hombres 
# y mujeres. Reporte e interprete sus resultados. 

str(base)
# 1 = H, 2 = M

# Factor - R lee 1/2 como var categorica
base$sexo<- as.factor(base$sexo)
str(base$sexo)

describe(base)

# usando el modelo configural y MLR (ya sabemos no hay norm. MV)
fit_config_grupo <-  sem(configural, data=base, estimator = "MLR", group="sexo")
summary(fit_config_grupo, fit.measures = T, standardized = T, rsquare = T)

# Invarianza fuerte (cargas fact iguales)
fit_debil_grupo <- sem(configural, data=base, std.lv=T, estimator = "MLR",
                       group="sexo", group.equal="loadings")
summary(fit_debil_grupo, fit.measures = T, standardized = T, rsquare = T)

# Invarianza fuerte (interceptos iguales)
fit_fuerte_grupo <- sem(configural, data=base, std.lv=T, estimator = "MLR", 
                       group="sexo", group.equal=c("loadings", "intercepts"))
summary(fit_fuerte_grupo, fit.measures = T, standardized = T, rsquare = T)

# Invarianza estricta (residuos iguales)
fit_estricta_grupo <- sem(configural, data=base, std.lv=T, estimator = "MLR", 
                          group="sexo", group.equal=c("loadings", "intercepts", 
                                                      "residuals"))
summary(fit_estricta_grupo, fit.measures = T, standardized = T, rsquare = T)

# Comparación de ajuste de los modelos (chi2)
anova(fit_config_grupo, fit_debil_grupo, fit_fuerte_grupo, fit_estricta_grupo)

# Resultados: El modelo es invariante completamente entre los grupos de sexo en
# el tiempo. Esto sugiere que la escala mide ansiedad igualmente entre hombres 
# y mujeres a lo largo del tiempo.

# ---- Analiza si hay diferencias signif en los niveles de ansiedad entre grupos ----
# Para pregunta numero 4: 4. Analiza si hay diferencias significa vas en los 
# niveles de ansiedad entre hombres y mujeres. 

fit_var_cov_grupo <- sem(configural, data=base, std.lv=T, estimator = "ML", 
                          group="sexo", group.equal=c("loadings", "intercepts", 
                                                      "residuals", "lv.variances", 
                                                      "lv.covariances"))
summary(fit_var_cov_grupo, fit.measures = T, standardized = T, rsquare = T)


fit_ans_sexo <- sem(mod0, data = base, group = "sexo", 
                        group.equal = c("loadings", "intercepts"), 
                        group.partial = "ansT2~1", estimator = "MLR")

summary(fit_ans_sexo, standardized = T, fit.measures = TRUE)

# Resultados: Para el grupo de mujeres, los interceptos de las VL no son 
# significativos, lo cual significa que no se diferencian de cero. Esto quiere
# decir que son estadisticamente iguales los niveles de ansiedad entre hombre y
# mujer.


