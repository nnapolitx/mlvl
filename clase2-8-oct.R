library(lavaan)
library(MVN)
library(psych)
getwd()
setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/clases/data")

base<- read.csv("sem2.csv", header = T, sep = ";", na=-9)
names(base)

str(base)

describe(base[3:14], skew =F) # descriptivos sin sesgo y curtosis

# Especificación del modelo

mod1<-'# Modelo de medición
 justif=~sj_01 +sj_02 +sj_03
 homoneg=~homo1_gay +homo2_gay +homo3_gay +
 homo4_gay+ homo5_gay+ homo6_gay+
 homo7_gay
 # Modelo estructural
 homoneg~justif +religiosidad
 justif~~ religiosidad'

# Estimación del modelo con MLR
# varianza de los factores fijas en 1 (std.lv = T)

fit1<-sem(mod1, estimator = "MLR", std.lv=T, sample.mean =T, data=base)

#índicesde ajuste, resultados estandarizados y R2
summary(fit1, fit.measures =T, standardized =T, rsquare =T)

base$sexo<- as.factor(base$sexo)
str(base$sexo)

# Estimación invarianza configural
# sobre el modelo especificado previamente "mod1"
config <- sem(mod1, data=base,
              estimator = "MLR",
              group="sexo") # Variable de agrupación

summary(config, fit.measures = T, standardized = T, rsquare = T)

#Invarianza débil(cargas factoriales fijas a iguales)
debil <-sem(mod1, data=base, std.lv=T, estimator = "MLR",
            group="sexo",
            group.equal="loadings")

# Invarianza fuerte (interceptosfijosaiguales)
fuerte <-sem(mod1, data=base, std.lv=T, estimator = "MLR",
             group="sexo",
             group.equal=c("loadings", "intercepts"))

# Invarianza estricta (residuos fijos a iguales)
estricta <-sem(mod1, data=base, std.lv=T, estimator = "MLR",
               group="sexo",
               group.equal=c("loadings", "intercepts", "residuals"))

# Comparación de ajuste de los modelos (chi2)
anova(config,debil, fuerte, estricta)

# ----Analisis de Invarianza----

base<-read.csv("invarian.csv", header = T, sep = ";")
names(base)

str(base)

mod2<-'habilT1=~ info1+ compren1+ simil1 +voca1'
fit2<-cfa(mod2, std.lv=T, estimator = "MLR",meanstructure=T, data=base)
summary(fit2,fit.measures =T, standardized =T, rsquare =T)

# Especificación y estimación del modelo T2
mod3<-'habilT2=~ info2+ compren2+ simil2 +voca2'
fit3<-cfa(mod3, std.lv=T, estimator = "MLR",meanstructure=T, data=base)
summary(fit3,fit.measures =T, standardized =T, rsquare =T)

configural<- '
 habilT1 =~ info1 + compren1 + simil1 + voca1
 habilT2 =~ info2 + compren2 + simil2 + voca2
 
 # interceptos libremente estimados
 info1 ~1
 compren1 ~1
 simil1 ~1
 voca1 ~1
 info2 ~1
 compren2 ~1
 simil2 ~1
 voca2 ~1
 
 # Residuos libremente estimados
 info1 ~~ info1
 compren1 ~~ compren1
 simil1 ~~ simil1
 voca1 ~~ voca1
 info2 ~~ info2
 compren2 ~~ compren2
 simil2 ~~ simil2
 voca2 ~~ voca2
 
 # Varianza de los factores fijos en 1 (identificación)
 habilT1 ~~ 1*habilT1
 habilT2 ~~ 1*habilT2
 
 # Medias de los factores fijos en 0 (identificación)
 habilT1 ~0*1
 habilT2 ~0*1
 
 # Residuos de los ítems en el tiempo correlacionados
 info1 ~~ info2
 compren1 ~~ compren2
 simil1 ~~ simil2
 voca1 ~~ voca2
 
 # Covarianza entre los factores
 habilT1 ~~ habilT2'

fit_configural <- cfa(configural, data = base, std.lv=T,
                      estimator = "MLR", mimic = "mplus")

summary(fit_configural, fit.measures = TRUE, standardized = T)

debil<- '
  habilT1 =~ NA*info1 + l1*info1+ l2*compren1 + l3*simil1 + l4*voca1
  habilT2 =~ NA*info2 + l1*info2+ l2*compren2 + l3*simil2 + l4*voca2

# interceptos libremente estimados
  info1 ~1
  compren1 ~1
  simil1 ~1
  voca1 ~1
  info2 ~1
  compren2 ~1
  simil2 ~1
  voca2 ~1

# Residuos libremente estimados
  info1 ~~ info1
  compren1 ~~ compren1
  simil1 ~~ simil1
  voca1 ~~ voca1
  info2 ~~ info2
  compren2 ~~ compren2
  simil2 ~~ simil2
  voca2 ~~ voca2
  
  # Varianza de los factores fijos en 1 (identificación)
  habilT1 ~~ 1*habilT1
  habilT2 ~~ habilT2

# Medias de los factores fijos en 0 (identificación)
  habilT1 ~0*1
  habilT2 ~0*1

# Residuos de los ítems en el tiempo correlacionados
  info1 ~~ info2
  compren1 ~~ compren2
  simil1 ~~ simil2
  voca1 ~~ voca2

# Covarianza entre los factores
  habilT1 ~~ habilT2'

fit_debil <-cfa(debil, data = base,
                estimator = "MLR", mimic = "mplus")

summary(fit_debil, fit.measures = TRUE, standardized =T)
 
anova(fit_configural,fit_debil)

lavTestScore(fit_debil)
# Results indicate to liberate/free item two (p2/p6)
# Remove the loading on compren1/2

debilP <- '
  habilT1 =~ NA*info1 + l1*info1+ compren1 + l3*simil1 + l4*voca1
  habilT2 =~ NA*info2 + l1*info2+ compren2 + l3*simil2 + l4*voca2

# interceptos libremente estimados
  info1 ~1
  compren1 ~1
  simil1 ~1
  voca1 ~1
  info2 ~1
  compren2 ~1
  simil2 ~1
  voca2 ~1

# Residuos libremente estimados
  info1 ~~ info1
  compren1 ~~ compren1
  simil1 ~~ simil1
  voca1 ~~ voca1
  info2 ~~ info2
  compren2 ~~ compren2
  simil2 ~~ simil2
  voca2 ~~ voca2
  
  # Varianza de los factores fijos en 1 (identificación)
  habilT1 ~~ 1*habilT1
  habilT2 ~~ habilT2

# Medias de los factores fijos en 0 (identificación)
  habilT1 ~0*1
  habilT2 ~0*1

# Residuos de los ítems en el tiempo correlacionados
  info1 ~~ info2
  compren1 ~~ compren2
  simil1 ~~ simil2
  voca1 ~~ voca2

# Covarianza entre los factores
  habilT1 ~~ habilT2'

fit_debilP <-cfa(debilP, data =base,
                 estimator = "MLR", mimic = "mplus")

summary(fit_debilP, fit.measures = TRUE, standardized =T)

anova(fit_configural, fit_debilP)


fuerte <- '
  habilT1 =~ NA*info1 + l1*info1+ compren1 + l3*simil1 + l4*voca1
  habilT2 =~ NA*info2 + l1*info2+ compren2 + l3*simil2 + l4*voca2

# interceptos libremente estimados
  info1 ~i1*1
  compren1 ~1
  simil1 ~i3*1
  voca1 ~i4*1
  info2 ~i1*1
  compren2 ~1
  simil2 ~i3*1
  voca2 ~i4*1

# Residuos libremente estimados
  info1 ~~ info1
  compren1 ~~ compren1
  simil1 ~~ simil1
  voca1 ~~ voca1
  info2 ~~ info2
  compren2 ~~ compren2
  simil2 ~~ simil2
  voca2 ~~ voca2
  
# Varianza de los factores fijos en 1 (identificación)
  habilT1 ~~ 1*habilT1
  habilT2 ~~ habilT2

# Medias de los factores fijos en 0 (identificación)
  habilT1 ~0*1
  habilT2 ~1

# Residuos de los ítems en el tiempo correlacionados
  info1 ~~ info2
  compren1 ~~ compren2
  simil1 ~~ simil2
  voca1 ~~ voca2

# Covarianza entre los factores
  habilT1 ~~ habilT2'
 
fit_fuerte <-cfa(fuerte, data =base,
 estimator = "MLR", mimic = "mplus")

summary(fit_fuerte, fit.measures = TRUE, standardized =T)

anova(fit_debilP,fit_fuerte)
# Indicates that we need to stick with weak invaraince. However we could
# continue to liberate/free up intercepts.

lavTestScore(fit_fuerte)



