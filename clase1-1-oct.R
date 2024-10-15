library(lavaan)
library(MVN)
library(psych)
getwd()
setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/clases/data")

base <- read.csv("sem.csv", header = T, sep = ";", na=-9)

describe(base[3:14])

mvn(base[4:14], desc=F)

mod1<-'justif=~ sj_01 +sj_02 +sj_03
  homoneg=~ homo1_gay + homo2_gay + homo3_gay + homo4_gay + homo5_gay + 
  homo6_gay + homo7_gay
  homoneg~ justif + religiosidad
  justif~~ religiosidad'

fit1 <- sem(mod1, estimator = 'MLR', std.lv = T, sample.mean = T, data = base)
summary(fit1, fit.measures =T, standardized = T, rsquare = T)

config <-sem(mod1, data=base, std.lv=F, estimator = "MLR", meanstructure = T,
             sample.mean =T, group="sexo")

summary(config, fit.measures =T, standardized =T, rsquare =T)

debil <-sem(mod1, data=base, std.lv=T, estimator = "MLR",
            group="sexo",
            group.equal="loadings")

fuerte <-sem(mod1, data=base, std.lv=T, estimator = "MLR",
             group="sexo",
             group.equal=c("loadings", "intercepts"))

estricta <-sem(mod1, data=base, std.lv=T, estimator = "MLR",
               group="sexo",
               group.equal=c("loadings", "intercepts", "residuals"))

# Comparación de ajuste de los modelos
anova(config,debil, fuerte, estricta)

